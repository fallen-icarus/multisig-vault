#!/bin/bash

# Closing the vault entails:
#   1. withdraw all remaining utxos in the vault (including ones with reference scripts)
#   2. burn beacons

# This can be done in one tx
dir="../assets/on-chain-files/" # Where datum and redeemer files are stored
tmpDir="../assets/tmp/"   # Where to store the tmp files: tx.body, protocol.json, tx.signed, etc.
datumFile="${dir}unitDatum.json"
vaultCloseRedeemerFile="${dir}vaultCloseRedeemer.json"
burnRedeemerFile="${dir}burnBeaconsRedeemer.json"
beaconSymbol=$(cat ${dir}beaconSymbol.txt)
keyToken="${beaconSymbol}.4b6579"
vaultToken="${beaconSymbol}.5661756c74"

cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic 1 \
  --change-address $(cat ../assets/wallets/01.addr) \
  --tx-in 2157bcbbf3c8e6bea3925077dd2c5313361b0e68f8c670bdf424a0c0b48cae73#0 \
  --spending-tx-in-reference 2157bcbbf3c8e6bea3925077dd2c5313361b0e68f8c670bdf424a0c0b48cae73#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-datum-file $datumFile \
  --spending-reference-tx-in-redeemer-file $vaultCloseRedeemerFile \
  --tx-in ac107310d3145c2df90c1783e04234213e771e0ac41a0cce8017345e7daedfc2#0 \
  --spending-tx-in-reference 2157bcbbf3c8e6bea3925077dd2c5313361b0e68f8c670bdf424a0c0b48cae73#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-datum-file $datumFile \
  --spending-reference-tx-in-redeemer-file $vaultCloseRedeemerFile \
  --tx-in ac107310d3145c2df90c1783e04234213e771e0ac41a0cce8017345e7daedfc2#1 \
  --spending-tx-in-reference 2157bcbbf3c8e6bea3925077dd2c5313361b0e68f8c670bdf424a0c0b48cae73#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-datum-file $datumFile \
  --spending-reference-tx-in-redeemer-file $vaultCloseRedeemerFile \
  --tx-in bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#2 \
  --tx-in 2157bcbbf3c8e6bea3925077dd2c5313361b0e68f8c670bdf424a0c0b48cae73#2 \
  --tx-in 2157bcbbf3c8e6bea3925077dd2c5313361b0e68f8c670bdf424a0c0b48cae73#3 \
  --mint "-3 ${keyToken} + -1 ${vaultToken}" \
  --mint-tx-in-reference ac107310d3145c2df90c1783e04234213e771e0ac41a0cce8017345e7daedfc2#1 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $burnRedeemerFile \
  --policy-id $beaconSymbol \
  --tx-in-collateral bc54229f0755611ba14a2679774a7c7d394b0a476e59c609035e06244e1572bb#0 \
  --required-signer-hash fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f \
  --required-signer-hash 45a453383335bc3e3afe2c8d39fa76e5e60dfe8cbd1ffc4184e8b785 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "../assets/wallets/01.skey" \
  --signing-key-file "../assets/wallets/03.skey" \
  --signing-key-file "../assets/wallets/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"