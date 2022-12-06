# As long as the vault contains the vault beacon the vault can always be found by someone who hold the key beacon.
# The asset name of the key beacon in the user wallet is the hexidecimal encoding for the word "Key" (4b6579).
# The key beacon shares the asset policy with the vault beacon and the asset name for the vault beacon is the
#    hexidecimal encoding for the word "Vault" (5661756c74).


# On mainnet koios.rest can be used to find all addresses that have the vault beacon (there should only be one address).
# curl -X GET "https://api.koios.rest/api/v0/asset_address_list?_asset_policy=<policy_name_here>&_asset_name=5661756c74" \
# -H "Accept: application/json" 

# On PreProd testnet the more centralized blockfrost api can be used (or an explorer)
blockfrostApiKey=$1
beaconPolicy=$2

curl -H "project_id: ${blockfrostApiKey}" https://cardano-preprod.blockfrost.io/api/v0/assets/"${beaconPolicy}5661756c74"/addresses