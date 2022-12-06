{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}

module MultiSigVault 
(
  vaultScript,
  vaultValidatorHash,
  beaconScript,
  beaconSymbol,
  stakingScript,
  writeScript,
  writeData,
  VaultSettings (..),
  BeaconSettings (..),
  VaultRedeemer (..),
  BeaconRedeemer (..),
  FromJSON (..)
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath) 
import qualified Prelude as Haskell

import           Cardano.Api hiding (Script,Value,TxOut)
import           Cardano.Api.Shelley   (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Plutus.V1.Ledger.Value (flattenValue)
import Ledger.Bytes (fromHex)
import Data.String (fromString)
import qualified Plutonomy
import Ledger.Value (noAdaValue)
import PlutusTx.Numeric as Num

-------------------------------------------------
-- Vault Settings
-------------------------------------------------
-- for use as an extra parameter
data VaultSettings = VaultSettings
  { sigs :: [PaymentPubKeyHash]
  , threshold :: Integer
  }

PlutusTx.makeLift ''VaultSettings

-- for parsing a json file that represents VaultSettings
instance FromJSON VaultSettings where
  parseJSON = withObject "VaultSettings" $ \v -> VaultSettings
    Haskell.<$> (Haskell.map ((\(Right x) -> x) . parsePubKey) Haskell.<$> v .: "sigs")
    Haskell.<*> v .: "threshold"

parsePubKey :: Haskell.String -> Either Haskell.String PaymentPubKeyHash
parsePubKey s = case fromHex (fromString s) of
  Right (LedgerBytes bytes') -> Right $ PaymentPubKeyHash $ PubKeyHash bytes'
  Left msg -> Left $ "could not convert: " <> msg

-------------------------------------------------
-- Vault Redeemer
-------------------------------------------------
data VaultRedeemer = Withdraw -- only allows withdrawing utxos that do not hold reference scripts
                   | Close    -- withdraw all utxos (including ones with reference scripts)

PlutusTx.unstableMakeIsData ''VaultRedeemer

-------------------------------------------------
-- Beacon Settings
-------------------------------------------------
-- combined VaultSettings with vault script hash
data BeaconSettings = BeaconSettings
  { vaultSettings :: VaultSettings
  , vaultHash :: ValidatorHash
  }

PlutusTx.makeLift ''BeaconSettings

-------------------------------------------------
-- Beacon Redeemer
-------------------------------------------------
data BeaconRedeemer 
  = Mint -- mint beacons but only send to the vaultHash in BeaconSettings or a pubkey in VaultSettings
  | Burn -- burn beacons but only if all beacons are present

PlutusTx.unstableMakeIsData ''BeaconRedeemer

-------------------------------------------------
-- Helpers
-------------------------------------------------
{-# INLINABLE sigThresholdMet #-}
sigThresholdMet :: VaultSettings -> TxInfo -> Bool
sigThresholdMet VaultSettings{..} info =
  (>= threshold) $ length $ filter (txSignedBy info . unPaymentPubKeyHash) sigs

-------------------------------------------------
-- On-Chain Vault
-------------------------------------------------
{-# INLINABLE mkVault #-}
-- parameterized to produce a unique address for each user
-- this enables delegation control over the vault
mkVault :: VaultSettings -> () -> VaultRedeemer -> ScriptContext -> Bool
mkVault s () r ScriptContext{scriptContextTxInfo = info} = case r of
  Withdraw ->
    -- ensure reference scripts aren't withdrawn (if any)
    traceIfFalse "inputs contain reference scripts" (null inputsWithReferenceScripts) &&
    -- ensure multisig threshold met
    traceIfFalse "multisig threshold not met" (sigThresholdMet s info)
  Close ->
    -- allow withdrawing everything
    -- ensure multisig threshold met
    traceIfFalse "multisig threshold not met" (sigThresholdMet s info)

  where
    inputsWithReferenceScripts :: [TxOut]
    inputsWithReferenceScripts = filter (isJust . txOutReferenceScript)
                               $ map txInInfoResolved
                               $ txInfoInputs info

data Vault
instance ValidatorTypes Vault where
  type instance RedeemerType Vault = VaultRedeemer
  type instance DatumType Vault = ()

vault :: VaultSettings -> Validator
vault vs = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Vault
    ($$(PlutusTx.compile [|| mkVault ||]) 
        `PlutusTx.applyCode` PlutusTx.liftCode vs)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator

vaultScript :: VaultSettings -> Script
vaultScript = unValidatorScript . vault

vaultValidatorHash :: VaultSettings -> ValidatorHash
vaultValidatorHash = Scripts.validatorHash . vault

-------------------------------------------------
-- On-Chain Beacons
-------------------------------------------------
mkBeacons :: BeaconSettings -> BeaconRedeemer -> ScriptContext -> Bool
mkBeacons BeaconSettings{..} r ScriptContext{scriptContextTxInfo = info} = case r of
  Mint ->
    -- ensure multisig threshold met
    traceIfFalse "multisig threshold not met" (sigThresholdMet vaultSettings info) &&
    -- 1 token per vault and 1 key per signer
    traceIfFalse "wrong number of tokens minted" allVaultAndKeysProduced &&
    -- vault token must go to script and keys must go to signers
    traceIfFalse "tokens not properly distributed; no other native tokens permitted in tx" properDistribution
    
  Burn ->
    -- ensure multisig threshold met
    traceIfFalse "multisig threshold not met" (sigThresholdMet vaultSettings info) &&
    -- ensure all tokens present for burning
    traceIfFalse "not all outstanding tokens burned" allVaultAndKeysBurned

  where
    -- only tokens; no ada
    valProduced :: Value
    valProduced = noAdaValue $ valueProduced info

    allVaultAndKeysProduced :: Bool
    allVaultAndKeysProduced = 
      let isVaultToken (_,tn,n) = tn == TokenName "Vault" && n == 1
          areKeyTokens (_,tn,n) = tn == TokenName "Key" && n == length (sigs vaultSettings)
      in all (\z -> isVaultToken z || areKeyTokens z) $ flattenValue valProduced

    allVaultAndKeysBurned :: Bool
    allVaultAndKeysBurned =
      let isVaultToken (_,tn,n) = tn == TokenName "Vault" && n == (Num.negate 1)
          areKeyTokens (_,tn,n) = tn == TokenName "Key" && n == (Num.negate $ length $ sigs vaultSettings)
      in all (\z -> isVaultToken z || areKeyTokens z) $ flattenValue valProduced

    properDistribution :: Bool
    properDistribution =
      let signersVal = map (noAdaValue . valuePaidTo info . unPaymentPubKeyHash) $ sigs vaultSettings
          isKeyToken [(_,tn,n)] = tn == TokenName "Key" && n == 1
          isKeyToken _          = False
          isVaultToken (_,tn,n) = tn == TokenName "Vault" && n == 1
          vaultVal = noAdaValue $ valueLockedBy info vaultHash
      in 
        -- one key to each member
        all (isKeyToken . flattenValue) signersVal &&
        -- vault token to vault
        all isVaultToken (flattenValue vaultVal) &&
        -- only tokens produced
        valProduced == ((mconcat signersVal) <> vaultVal)

beacons :: BeaconSettings -> MintingPolicy
beacons s = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode s)
  where
    wrap = mkUntypedMintingPolicy . mkBeacons

beaconScript :: BeaconSettings -> Script
beaconScript = unMintingPolicyScript . beacons

beaconSymbol :: BeaconSettings -> CurrencySymbol
beaconSymbol = scriptCurrencySymbol . beacons

-------------------------------------------------
-- On-Chain Staking
-------------------------------------------------
-- anything is allowed as long as multisig threshold met
mkStaking :: VaultSettings -> () -> ScriptContext -> Bool
mkStaking s () ScriptContext{scriptContextPurpose = purpose,scriptContextTxInfo = info} = 
  case purpose of
    Rewarding _  -> traceIfFalse "multisig threshold not met" (sigThresholdMet s info)
    Certifying _ -> traceIfFalse "multisig threshold not met" (sigThresholdMet s info)
    _            -> False

staking :: VaultSettings -> StakeValidator
staking s = mkStakeValidatorScript
  ($$(PlutusTx.compile [|| mkUntypedStakeValidator . mkStaking ||])
    `PlutusTx.applyCode` PlutusTx.liftCode s)

stakingScript :: VaultSettings -> Script
stakingScript = unStakeValidatorScript . staking

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file 
               . encode 
               . scriptDataToJson ScriptDataJsonDetailedSchema 
               . dataToScriptData 
               . PlutusTx.toData

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                        $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file d = writeJSON file d