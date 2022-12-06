#!/bin/bash

# Delegating the vault entails:
#   1. registering the vault staking address
#   2. delegating to a pool

# As of now, the change for this tx must not go to the script due to being unable to attach
# a datum to the output of --change-address in cardano-cli. For this reason, paying for the
# delegation deposit and fee must come from a payment pub key.

dir="../assets/on-chain-files/" # Where datum and redeemer files are stored
tmpDir="../assets/tmp/"   # Where to store the tmp files: tx.body, protocol.json, tx.signed, etc.
stakingRedeemerFile="${dir}unitRedeemer.json"
stakingPlutusFile="${dir}staking.plutus"

cardano-cli stake-address registration-certificate \
  --stake-script-file $stakingPlutusFile \
  --out-file "${tmpDir}registration.cert"

cardano-cli stake-address delegation-certificate \
  --stake-script-file $stakingPlutusFile \
  --stake-pool-id pool13la5erny3srx9u4fz9tujtl2490350f89r4w4qjhk0vdjmuv78v \
  --out-file "${tmpDir}delegation.cert"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 5077c63e6b5dd78bee1e5985146b4a82bcd93ec7d150aa343718be8b329913bb#1 \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --certificate-file "${tmpDir}registration.cert" \
  --certificate-file "${tmpDir}delegation.cert" \
  --certificate-script-file $stakingPlutusFile \
  --certificate-redeemer-file $stakingRedeemerFile \
  --required-signer-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
  --required-signer-hash 45a453383335bc3e3afe2c8d39fa76e5e60dfe8cbd1ffc4184e8b785 \
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