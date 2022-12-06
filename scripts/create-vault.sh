#!/bin/bash

# Initializing the vault entails the following steps:
#   1. Executing the haskell program to generate the necessary files
#   2. Building the vault address that can also be staked
#   3. Minting the beacon tokens and storing the spending, staking, and minting reference scripts 
#      inside the new address. This requires a deposit since reference scripts
#      must be stored with a set amount of ADA. The ADA can be recovered
#      by simply spending the utxos that contain the reference scripts.

# Step 1: Executing the haskell program
dir="../assets/on-chain-files/"      # Where should the files be saved
vaultConfigFile="../settings.json"   # Where is the config file

cabal run multisig-vault -- --scripts --directory $dir --config-file $vaultConfigFile


# Step 2: Building the vault address with staking
vaultPlutusFile="${dir}vault.plutus"
stakingPlutusFile="${dir}staking.plutus"
vaultPaymentAddrFile="../assets/wallets/vaultPayment.addr"  # Where to save the vault payment address
vaultStakingAddrFile="../assets/wallets/vaultStaking.addr"  # Where to save the vault staking address

cardano-cli address build \
  --payment-script-file $vaultPlutusFile \
  --stake-script-file $stakingPlutusFile \
  --testnet-magic 1 \
  --out-file $vaultPaymentAddrFile

cardano-cli stake-address build \
  --stake-script-file $stakingPlutusFile \
  --testnet-magic 1 \
  --out-file $vaultStakingAddrFile


# Step 3: Minting the beacon tokens and storing the reference scripts in the vault
# Due to tx size limitations, this must be broken up into two txs
# The first tx will mint and store the vault token with the spending reference script in the vault
# The other tx will store the other two reference scripts in the vault
beaconPlutusFile="${dir}beacons.plutus"
datumFile="${dir}unitDatum.json"
mintRedeemer="${dir}mintBeaconsRedeemer.json"
beaconSymbol=$(cat ${dir}beaconSymbol.txt)
keyToken="${beaconSymbol}.4b6579"
vaultToken="${beaconSymbol}.5661756c74"
tmpDir="../assets/tmp/"  # Where to store the tmp files: tx.body, protocol.json, tx.signed, etc.
changeAddr="../assets/wallets/01.addr"  # Where is the change address file

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#3 \
  --tx-out "$(cat ${vaultPaymentAddrFile}) 20000000 lovelace + 1 ${vaultToken}" \
  --tx-out-reference-script-file $vaultPlutusFile \
  --tx-out-datum-hash-file $datumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) 10000000 lovelace + 1 ${keyToken}" \
  --tx-out "$(cat ../assets/wallets/02.addr) 10000000 lovelace + 1 ${keyToken}" \
  --tx-out "$(cat ../assets/wallets/03.addr) 10000000 lovelace + 1 ${keyToken}" \
  --mint "3 ${keyToken} + 1 ${vaultToken}" \
  --mint-script-file $beaconPlutusFile \
  --mint-redeemer-file $mintRedeemer \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --required-signer-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
  --required-signer-hash 45a453383335bc3e3afe2c8d39fa76e5e60dfe8cbd1ffc4184e8b785 \
  --change-address $(cat ${changeAddr}) \
  --protocol-params-file "${tmpDir}protocol.json" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "../assets/wallets/01.skey" \
  --signing-key-file "../assets/wallets/03.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"

cardano-cli transaction build \
  --babbage-era \
  --tx-in bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#1 \
  --tx-out "$(cat ${vaultPaymentAddrFile}) 20000001 lovelace" \
  --tx-out-reference-script-file $stakingPlutusFile \
  --tx-out-datum-hash-file $datumFile \
  --tx-out "$(cat ${vaultPaymentAddrFile}) 20000002 lovelace" \
  --tx-out-reference-script-file $beaconPlutusFile \
  --tx-out-datum-hash-file $datumFile \
  --change-address $(cat ../assets/wallets/01.addr) \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "../assets/wallets/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"