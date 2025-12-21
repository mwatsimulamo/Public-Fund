# Public Fund Plutus Contract

A Plutus smart contract example implementing a public fund mechanism on the Cardano blockchain. This contract allows a depositor to lock funds that require approval from multiple officials before they can be released to a designated recipient. If insufficient approvals are obtained by the deadline, the depositor can refund the funds.

## Features

- **Multi-signature approvals**: Requires a specified number of approvals from authorized officials
- **Deadline enforcement**: All operations must occur within a defined time window
- **Refund mechanism**: Depositor can reclaim funds if deadline passes without sufficient approvals
- **Flexible configuration**: Datum includes officials list, required approvals count, deadline, and parties involved

## Contract Logic

### Datum Structure
- `officials`: List of public key hashes authorized to approve
- `requiredApprovals`: Minimum number of approvals needed to release funds
- `deadline`: POSIX timestamp after which refund becomes possible
- `depositor`: Public key hash of the fund depositor
- `recipient`: Public key hash of the intended recipient
- `approvalsCount`: Current number of approvals received

### Redeemer Actions

1. **Approve**: Officials can approve the fund (increments approval count)
   - Must be signed by an official
   - Must be before deadline
   - Cannot exceed required approvals

2. **Release**: Recipient can claim funds after sufficient approvals
   - Must have enough approvals
   - Must be before deadline

3. **Refund**: Depositor can reclaim funds after deadline
   - Must be after deadline
   - Must be signed by depositor
   - Must have insufficient approvals

## Prerequisites

- Haskell toolchain (GHC 8.10.7 or compatible)
- Cabal
- Plutus toolchain and dependencies

## Installation

Clone the repository and navigate to the project directory:

```bash
git clone <repository-url>
cd public-fund
```

Install dependencies:

```bash
cabal update
cabal build
```

## Usage

### Running Tests and Generating Script

The main executable compiles the Plutus script, runs unit tests, and generates the compiled script file:

```bash
cabal run public-fund-spec-exe
```

This will:
- Compile the validator
- Run comprehensive test suite
- Generate `assets/public_fund.plutus` (the compiled Plutus script)
- Generate `assets/sample_fund_datum.cbor` (sample datum for testing)

### Test Coverage

The test suite includes:
- Validator compilation verification
- Datum and redeemer serialization tests
- Logic validation for all contract actions
- Edge cases and failure scenarios

## Project Structure

```
public-fund/
├── app/Main.hs              # Main executable with validator, tests, and script generation
├── src/
│   ├── PublicFund.hs        # Library module
│   ├── Utilities.hs         # Utility functions
│   └── ValidatorLogic.hs    # Validator logic (generated)
├── tests/                   # Test files
├── assets/                  # Generated artifacts (.plutus files, etc.)
├── Tutorial.md              # Detailed tutorial
├── Tutorial.html            # HTML version of tutorial
└── public-fund.cabal        # Cabal configuration
```

## Development

This project is part of the Plutus starter kit and demonstrates best practices for:
- Plutus smart contract development
- Unit testing with Tasty
- Script compilation and serialization
- Datum/redeemer design patterns

## Related Resources

- [Plutus Documentation](https://plutus.readthedocs.io/)
- [Cardano Developer Portal](https://developers.cardano.org/)
- [Coxy Plutus Playground](Tutorial.html) - UI tool for generating similar contracts

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Authors

- Israel Ahunanay