#!/bin/bash

# Depositing funds to the vault simply requires sending the amount with the unit datum

dir="../assets/on-chain-files/" # Where datum and redeemer files are stored
tmpDir="../assets/tmp/"   # Where to store the tmp files: tx.body, protocol.json, tx.signed, etc.
vaultPaymentAddrFile="../assets/wallets/vaultPayment.addr" # Where is the vault address file
datumFile="${dir}unitDatum.json"

cardano-cli transaction build \
  --babbage-era \
  --tx-in bb118468b865e268d1532d909d07e17b5d07e2787e30b6261b90441654cda230#4 \
  --tx-out "$(cat ${vaultPaymentAddrFile}) 20000000 lovelace" \
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