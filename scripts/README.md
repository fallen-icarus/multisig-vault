# Example Scripts
All scripts assume a preproduction testnet is running locally and the socket is exported.

## Getting Started
You will need to edit each script to customize them to your situation. They are heavily commented to help with this process.

A lookup-vault script is included that can use the beacon tokens to lookup the vault address using blockfrost's api.

## Approximate Transaction Fees for Each Action (YMMV)
|Action|Tx Fee|
|----|---------------|
|Creating Vault w/ ref scripts (2 txs)|1.018581 ADA + 0.418809 ADA|
|Delegation|0.354412 ADA|
|Deregistering staking|testing|
|Withdrawing Rewards Tx Fee|testing|
|Depositing Funds|0.168581 ADA|
|Withdrawing Funds|0.228455 ADA|
|Closing Vault|0.572464 ADA|