#!/bin/bash

# Withdrawing funds from the vault can be done using the reference scripts

dir="../assets/on-chain-files/" # Where datum and redeemer files are stored
tmpDir="../assets/tmp/"   # Where to store the tmp files: tx.body, protocol.json, tx.signed, etc.
datumFile="${dir}unitDatum.json"
redeemerFile="${dir}vaultWithdrawRedeemer.json"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in 5077c63e6b5dd78bee1e5985146b4a82bcd93ec7d150aa343718be8b329913bb#0 \
  --spending-tx-in-reference bb118468b865e268d1532d909d07e17b5d07e2787e30b6261b90441654cda230#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-datum-file $datumFile \
  --spending-reference-tx-in-redeemer-file $redeemerFile \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --required-signer-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
  --required-signer-hash 45a453383335bc3e3afe2c8d39fa76e5e60dfe8cbd1ffc4184e8b785 \
  --protocol-params-file "${tmpDir}protocol.json" \
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