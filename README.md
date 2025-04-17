# Public Infrastructure Bond (PIB) Platform

A decentralized platform that allows citizens to invest in local infrastructure projects and earn yield on their investments.

## Overview

The PIB platform enables:

1. Project creators to issue infrastructure bonds with specific funding targets
2. Citizens to invest in these bonds and earn yield
3. Transparent tracking of bond status and investments
4. Automated yield distribution upon project completion

## Smart Contract Features

- **Bond Creation**: Create infrastructure bonds with customizable parameters
- **Investment**: Citizens can invest STX in active bonds
- **Yield Calculation**: Automatic calculation of yields based on investment amount and duration
- **Bond Lifecycle Management**: Activate, complete, and cancel bonds
- **Investor Management**: Track investors and their investments
- **Yield Distribution**: Distribute yields to investors upon project completion

## Usage

### For Project Creators

1. **Create a Bond**
   ```
   (contract-call? .PIB create-bond "City Park Renovation" "Renovation of the central park with new facilities" u1000000000 u500 u10000)
   ```
   Parameters:
   - Name (string-ascii 100)
   - Description (string-ascii 500)
   - Target amount in microSTX
   - Yield rate (basis points, e.g., 500 = 5%)
   - Duration in blocks

2. **Activate a Bond**
   ```
   (contract-call? .PIB activate-bond u1)
   ```

3. **Complete a Bond** (after expiration)
   ```
   (contract-call? .PIB complete-bond u1)
   ```

4. **Cancel a Bond** (if needed)
   ```
   (contract-call? .PIB cancel-bond u1)
   ```

5. **Refund Investors** (if bond is cancelled)
   ```
   (contract-call? .PIB refund-investors u1)
   ```

### For Investors

1. **Invest in a Bond**
   ```
   (contract-call? .PIB invest-in-bond u1 u10000000)
   ```
   Parameters:
   - Bond ID
   - Amount in microSTX

2. **Claim Yield** (after bond completion)
   ```
   (contract-call? .PIB claim-yield u1)
   ```

### Read-Only Functions

1. **Get Bond Details**
   ```
   (contract-call? .PIB get-bond u1)
   ```

2. **Get Investment Details**
   ```
   (contract-call? .PIB get-investment u1 tx-sender)
   ```

3. **Get Bond Investors**
   ```
   (contract-call? .PIB get-bond-investors u1)
   ```

## Yield Calculation

Yield is calculated using the formula:
```
yield = (investment_amount * yield_rate * duration_blocks) / 10000
```

For example, with:
- Investment: 1000 STX (1,000,000,000 microSTX)
- Yield rate: 500 (5%)
- Duration: 10000 blocks

The yield would be 50 STX (50,000,000 microSTX).

## Error Codes

- `u100`: Owner only operation
- `u101`: Bond not found
- `u102`: Already exists
- `u103`: Unauthorized operation
- `u104`: Bond already active
- `u105`: Bond not active
- `u106`: Insufficient funds
- `u107`: Bond expired
- `u108`: Bond not expired
- `u109`: Bond already fully funded
- `u110`: Bond not funded
- `u111`: Invalid amount
- `u112`: Invalid yield rate
- `u113`: Invalid duration
- `u114`: No investment found
