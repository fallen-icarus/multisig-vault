# On-Chain Multisig Vault

## Features
1. Unique vault address for every multisig
2. Thanks to number 1: delegation and rewards controlled by the multisig
3. Easily auditable by every multisig participant thanks to beacon tokens
4. Reference scripts stored in the vault itself (optional)

### Unique Vault Address
Alice's and Bob's vault will have a different address than Carol's and Alice's vault. This means that Alice and Bob can delegate their vault to Pool1 while Carol and Alice can delegate their vault to Pool2. Further, if Alice, Bob, and Carol create a vault together with a required threshold of 2 signatures, this will result in a different address than if they required a threshold of 3 signatures.

TLDR: A slight change in the vault configuration will result in a completely different vault address

### Delegation and Rewards Controlled By Multisig
All delegation actions (registering, withdrawing rewards, etc.) are controlled by the vault's multisig.

### Easily Auditable Due to Beacon Tokens
The vault will contain a vault beacon token and each multisig participant will have a key beacon token. By using these beacon tokens, each participant can always find the vault address even if they forgot the address. See How To Use Beacon Tokens below.

### Reference Scripts Stored in the Vault (Optional)
The vault can hold the reference scripts for spending, staking, and minting/burning the beacon tokens. This means that every participant will always have the actual plutus scripts at their fingertips.

## How To Use Beacon Tokens
Every token on Cardano contains a policy ID, a token name, and an amount. You can think of it as a triple like (policyID,tokenName,number). Using the policy ID and token name, you can find every address that holds a token of that policy ID and token name. For example, on mainnet you can use the [Koios api](https://api.koios.rest/#get-/asset_address_list). (They do not offer one for testnet so you will need to either use an explorer or the blockfrost api.)

When looking at an tokens on cardano, you will see tokens like: `1 4a152d8f80c0993262250b776766eab91223a72ab4c83217ccc23e44.4b6579`
This means 1 token with the policy id of 4a152d8f80c0993262250b776766eab91223a72ab4c83217ccc23e44 and the token name of 4b6579. The token name is the hexadecimal encoding of a word.

For the multisig-vault, every vault will have a unique policy id. The vault token name will be the hexidecimal encoding for the word "Vault" (5661756c74) and the key token name will be the hexidecimal encoding for the word "Key" (4b6579). Knowing this a user can look at the tokens at their address and find the token with the token name for "Key". Then using that policy id and the token name for "Vault" the Koios api above can be used to find the vault address. As long as they maintain custody of the key token, the vault address will always be just a query away.

## The On-Chain Logic

### Vault Spending
There are two possible actions for the vault: Withdrawing and Closing.

#### Withdrawing
Withdrawing allows withdrawing any utxo inside the vault as long as all of the following are true:
1. The multisig threshold is met
2. A utxo containing a reference script is not spent

#### Closing
Closing the vault allows spending any utxo inside the vault, including those with reference scripts, as long as the multisig threshold is met.

#### Important Note
As of right now, there is no check to make sure the vault token is not withdrawn from the vault. The vault token is deliberately stored with the spending reference script which protects it from being spent unless closing the vault. If reference scripts aren't used, care must be taken to not accidentally withdraw the vault token. A future version of the multisig vault may address this shortcoming.

### Staking
All staking actions (withdrawing rewards,delegating,etc) are allowed as long as the multisig threshold is met.

### Beacon
There are two possible actions for the beacon script: Minting and Burning.

#### Minting Beacons
Minting beacons has the following requirements:
1. The multisig threshold is met
2. One vault token is minted
3. One key token is minted per possible signer in the multisig settings
4. The vault token must go to the proper vault address (it is hardcoded into the minting policy at compile time)
5. One key token must go to every possible signer in the multisig settings (their payment pub keys are hardcoded into the minting policy at compile time)

#### Burning Beacons
Burning beacons has the following requirements:
1. The multisig threshold is met
2. All outstanding beacon tokens are present to burn

## Getting Started
In order for the haskell program to properly compile the plutus scripts, you need to do the following steps:
1. Clone the iohk [plutus-apps repository](https://github.com/input-output-hk/plutus-apps)
2. `cd` into the plutus-apps repo
3. checkout the `v1.0.0-alpha1` tag
4. execute `nix-shell` from within the plutus-app repo (this may take a while)
5. `cd` into this repo

Now the program should properly compile the scripts. If you would like to use an IDE like VScode, launch it from the terminal while in the nix-shell. Everything should work as long as it was launched from within the nix-shell.

The [settings.json file](https://github.com/fallen-icarus/multisig-vault/blob/main/settings.json) has a template for how to format your desired vault settings. Then checkout the scripts directory to see some example actions. Everyone unique settings.json file will have a unique vault address.