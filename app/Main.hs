{-# LANGUAGE RecordWildCards #-}

module Main where

import MultiSigVault
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Options.Applicative

-------------------------------------------------
-- Creating Scripts and Related Files
-------------------------------------------------
parseVaultSettings :: FilePath -> IO VaultSettings
parseVaultSettings file = do
  s <- decode <$> B.readFile file :: IO (Maybe VaultSettings)
  case s of
    Nothing -> error "Error parsing vault settings file"
    Just s' -> return s'

createScripts :: FilePath -> VaultSettings -> IO ()
createScripts dir vs = do
  let vaultPolicy = vaultScript vs
      beaconSettings = BeaconSettings
        { vaultSettings = vs
        , vaultHash = vaultValidatorHash vs
        }
      beaconPolicy = beaconScript beaconSettings
      beaconCurrencyName = beaconSymbol beaconSettings
      stakingPolicy = stakingScript vs
  
  -- create plutus scripts
  void $ writeScript (dir <> "vault.plutus") vaultPolicy
  void $ writeScript (dir <> "beacons.plutus") beaconPolicy
  void $ writeScript (dir <> "staking.plutus") stakingPolicy

  -- save beacon symbol
  writeFile (dir <> "beaconSymbol.txt") $ show beaconCurrencyName

createDatumsAndRedeemers :: FilePath -> IO ()
createDatumsAndRedeemers dir = do
  writeData (dir <> "unitRedeemer.json") ()
  writeData (dir <> "unitDatum.json") ()
  writeData (dir <> "mintBeaconsRedeemer.json") Mint
  writeData (dir <> "burnBeaconsRedeemer.json") Burn
  writeData (dir <> "vaultWithdrawRedeemer.json") Withdraw
  writeData (dir <> "vaultCloseRedeemer.json") Close

-------------------------------------------------
-- Command Line Parser
-------------------------------------------------
data Input = Input
  { scripts :: Bool -- If False, only creates datums/redeemers; if True, also creates script files
  , directory :: FilePath
  , vaultConfigFile :: FilePath
  }

inputParser :: Parser Input
inputParser = Input
  <$> flag False True (long "scripts" <> short 's' <> help "also create script files")
  <*> strOption (long "directory" <> short 'd' <> help "directory to save files")
  <*> strOption (long "config-file" <> help "json file for vault settings")

main :: IO ()
main = do
  let opts = info (inputParser <**> helper) (fullDesc <> progDesc "Generate multisig vault files")
  Input{..} <- execParser opts
  
  vs <- parseVaultSettings vaultConfigFile

  when scripts $ createScripts directory vs
  createDatumsAndRedeemers directory
