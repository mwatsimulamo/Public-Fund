# Public Fund Escrow Smart Contract (Anti-Corruption)

## Overview

This project implements a **Plutus V2 escrow smart contract** for public fund management with anti-corruption features. It provides a secure mechanism where funds are locked and can only be released after approvals from multiple designated officials before a deadline. If approvals are insufficient by the deadline, the funds are safely refunded to the depositor.  

The repository includes:  
- On-chain validator logic (`PublicFund.hs`)  
- Off-chain contract endpoints (`Main.hs`)  
- Emulator tests demonstrating success, refund, and failure scenarios  

---

## Problem

Managing public funds presents risks such as:  
- Misappropriation of funds due to weak governance  
- Lack of accountability when multiple parties are involved  
- Double-spending or approval manipulation  

This contract addresses these risks using **multi-signature approvals, deadline enforcement, and unique official signatures**, ensuring transparency and collective accountability.

---

## Design

### On-Chain Validator (`PublicFund.hs`)

**Datum (`EscrowDatum`)**:
- `edDepositor :: PubKeyHash` – Wallet depositing funds  
- `edBeneficiary :: PubKeyHash` – Wallet eligible to receive funds  
- `edOfficials :: [PubKeyHash]` – Authorized officials  
- `edApprovals :: [PubKeyHash]` – Collected approvals  
- `edRequired :: Integer` – Minimum approvals for release  
- `edDeadline :: POSIXTime` – Deadline for approvals  

**Redeemer (`EscrowAction`)**:
- `Approve` – Official approves transaction  
- `Release` – Beneficiary claims funds after quorum  
- `Refund` – Depositor recovers funds after deadline if approvals insufficient  

**Validator Rules**:
- Approve: Must be an authorized official, before deadline, and not already approved  
- Release: Must meet approval threshold, signed by beneficiary, before deadline  
- Refund: Only after deadline, approvals insufficient, signed by depositor  

### Off-Chain Contract (`Main.hs`)

**Endpoints**:
- `approve` – Official submits approval  
- `release` – Beneficiary claims funds  
- `refund` – Depositor reclaims funds after deadline  

**Emulator Tests**:
- Demonstrates full workflow, including:  
  - Successful release after quorum approvals  
  - Refund after deadline with insufficient approvals  
  - Failure scenarios: double approval, early release, unauthorized signer  

---

## Limitations

- Officials and quorum are **fixed at contract creation**  
- No emergency override or partial refund support  
- Deadline enforcement depends on blockchain slots  
- No real-world oracle integration or frontend UI included  

---

## How to Run

### Prerequisites
- Plutus development environment  
- GHC 8.10 or later  
- Required Plutus libraries  

### Build & Test
```bash
# Build the project
cabal build

# Run emulator tests
cabal test escrow-emulator
