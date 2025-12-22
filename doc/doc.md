# Public Fund Release Smart Contract (Anti-Corruption)

## Project Title

**Public Fund Release Smart Contract (Anti-Corruption)**

## Overview

This project implements a Plutus V2 on-chain validator that locks public funds and only allows release when a quorum of designated officials approve before a deadline. If the required approvals are not reached by the deadline, the depositor may refund the funds. The repository contains the on-chain validator, utilities to write the compiled `.plutus` file and a sample datum, plus a comprehensive unit test suite built with the Haskell testing library `tasty` (and `HUnit`) to validate compilation, serialization, validator logic, and artifact generation.

This documentation follows the CP108 Final Project requirements while focusing on the unit testing approach used (`tasty` + `HUnit`) rather than emulator endpoints.

---

## Design Goals

* Lock funds in a Plutus script so that only valid transitions are allowed.
* Implement `n-of-m` approval governance: funds require a configurable number of official approvals to be released to the recipient before the deadline.
* Allow depositor refund after the deadline if approvals < required.
* Keep on-chain datum small and verifiable; perform authorization checks via signer verification.
* Provide clear, testable logic and artifact generation for deployment.

---

## Contract Specification

### Datum: `FundDatum`

Represents the current state of the fund locked by the script.

Fields and rationale:

* `officials :: [PubKeyHash]`

  * A compact list of `PubKeyHash` values for the officials who are permitted to approve release operations. Keeping them in the datum allows the script to validate signers on-chain with no off-chain lookup.
* `requiredApprovals :: Integer`

  * The quorum threshold `n` for an `n-of-m` approval scheme. This is checked against the recorded `approvalsCount`.
* `deadline :: POSIXTime`

  * The absolute cutoff timestamp. Approvals and release may only occur before this time; refunds may occur after.
* `depositor :: PubKeyHash`

  * The address that must sign refunds after a successful expiry (if approvals < required).
* `recipient :: PubKeyHash`

  * The intended receiving address when a release succeeds. The contract logic enforces that funds are released to this address.
* `approvalsCount :: Integer`

  * A compact, numeric record of how many approvals have been recorded. This keeps the datum small, at the cost of not preventing double-approvals on-chain.

`FundDatum` is made serializable to/from builtin data with `PlutusTx.unstableMakeIsData`.

### Redeemer: `FundRedeemer`

Actions allowed by the validator (indexed for stable on-chain tags):

* `Approve` (index 0) — a designated official signs to increment approval count.
* `Release` (index 1) — final action to transfer funds to `recipient` if `approvalsCount >= requiredApprovals` and inside the deadline.
* `Refund`  (index 2) — depositor calls after deadline when quorum was not reached to recover funds.

`FundRedeemer` is serialized with `PlutusTx.makeIsDataIndexed` to fix numeric tags.

---

## Validator Logic (detailed)

The validator dispatches by redeemer to three independent validators with a shared set of inputs:

1. **Approve**

   * Preconditions:

     * Transaction valid range must be *before* `deadline`. Implemented as `Interval.to deadline` contained in `txInfoValidRange`.
     * One or more transaction signatories must be in the `officials` list.
     * `approvalsCount + 1 < requiredApprovals` — approve may only be called while the threshold has not yet been reached; this makes `Approve` an *increment* operation rather than the final releasing operation.
   * Outcome: returns `True` only if all preconditions hold; otherwise fails.

2. **Release**

   * Preconditions:

     * Transaction valid range must be *before* `deadline`.
     * `approvalsCount >= requiredApprovals`.
   * Outcome: allows an output that transfers funds to `recipient`. In the simplified validator used for testing, the logical gate is this boolean check — full accounting of outputs is handled off-chain or can be asserted by extended on-chain checks.

3. **Refund**

   * Preconditions:

     * Transaction valid range must be *after* `deadline`. Implemented with `Interval.from (deadline + 1)` to ensure a strict after-deadline window.
     * The `depositor` must be a transaction signer (`txSignedBy info depositor`).
     * `approvalsCount < requiredApprovals` — refunds allowed only when quorum wasn't reached.
   * Outcome: allows the depositor to reclaim funds.

Implementation notes:

* All time checks use `txInfoValidRange` with `Plutus.V1.Ledger.Interval` utilities.
* Signer checks use `txInfoSignatories` and `txSignedBy` for depositor verification.
* The typed validator is wrapped in a `wrapper` function to convert `BuiltinData` to typed values and call the typed validator; compilation uses `mkValidatorScript` and Template Haskell splice `$$`.

---

## Artifact Generation (how it works)

The project contains helpers to generate deployable artifacts:

* `writeValidator :: FilePath -> Validator -> IO ()`

  * Serializes the `Validator` with `serialise`, converts to `ShortByteString`, wraps into `PlutusScriptSerialised`, and writes via `writeFileTextEnvelope` from `cardano-api`.
  * Produces a `.plutus` file usable by deployment tooling.

* `saveDatum :: FilePath -> FundDatum -> IO ()`

  * Converts the datum to Plutus `Data` (`PlutusTx.toData`), serializes it with `serialise` and writes as CBOR to a file. Useful for demos or for constructing transactions that consume the script UTxO.

The `main` function demonstrates usage: generates a sample datum, writes `./assets/public_fund.plutus` and `./assets/sample_fund_datum.cbor`, then runs the test suite.

---

## Testing Strategy — **tasty + HUnit** (extensive)

This repository uses `tasty` as the test orchestration framework and `HUnit` for assertions. Below is a thorough description of the testing approach, the coverage matrix, and how each test maps to the contract requirements.

### Testing goals

* Verify datum/redeemer (de-)serialization correctness.
* Validate logical correctness of `Approve`, `Release`, and `Refund` behaviors with both positive and negative test cases.
* Ensure validator compiles and the final `.plutus` artifact is generated without error.
* Provide reproducible, deterministic unit tests that can run in CI.

### Test groups

* **Validator compilation** — a smoke test that the `Validator` value can be constructed and compiled to a script value in memory.
* **Serialization tests** — roundtrip checks for `FundDatum` and `FundRedeemer` using `toBuiltinData` and `unsafeFromBuiltinData` to ensure the data encodings are stable.
* **Validator logic tests** — a large matrix of behavioral tests that confirm acceptance or rejection in realistic signer/time scenarios.
* **Plutus script generation** — writes the `.plutus` file and asserts that no `Left` error was returned by `writeFileTextEnvelope`.

### Mocking and test scaffolding

* **Mock public keys**: deterministic `PubKeyHash` values are constructed from short `ByteString` constants to keep tests readable and deterministic (`mockPKH1`, `mockPKH2`, `mockPKH3`, `mockDepositor`, `mockRecipient`, `mockNonOfficial`).
* **Mock ScriptContext**: `createMockContext :: [PubKeyHash] -> POSIXTimeRange -> ScriptContext` builds a synthetic `ScriptContext` containing a `TxInfo` with fields relevant to the validator: `txInfoSignatories`, `txInfoValidRange`, and an example `txInfoId`.
* **POSIXTime ranges**: Use `Interval.interval start end` and `Interval.from t` to craft before/after deadline ranges.

### Test matrix (expanded)

Below is a condensed but explicit matrix of scenarios tested. Each row corresponds to a `TestCase` in the `Validator Logic Tests` group.

| Scenario ID | Action  | Signers (example)     | Time window     | Datum approvals | Expected result | Purpose                                                 |
| ----------- | ------- | --------------------- | --------------- | --------------- | --------------- | ------------------------------------------------------- |
| V1          | Approve | [mockPKH1] (official) | before deadline | 0               | pass            | official can cast approval                              |
| V2          | Approve | [mockNonOfficial]     | before deadline | 0               | fail            | non-official cannot approve                             |
| V3          | Approve | [mockPKH1]            | after deadline  | 0               | fail            | approvals disallowed after deadline                     |
| V4          | Release | []                    | before deadline | 0               | fail            | insufficient approvals blocks release                   |
| V5          | Release | []                    | before deadline | required (2)    | pass            | release allowed with sufficient approvals               |
| V6          | Release | []                    | after deadline  | required (2)    | fail            | release not allowed after deadline                      |
| V7          | Refund  | [mockDepositor]       | before deadline | 0               | fail            | cannot refund before deadline                           |
| V8          | Refund  | [mockDepositor]       | after deadline  | 0               | pass            | depositor can refund after deadline when quorum not met |
| V9          | Refund  | [mockPKH1]            | after deadline  | 0               | fail            | non-depositor cannot refund                             |
| V10         | Refund  | [mockDepositor]       | after deadline  | required (2)    | fail            | refund blocked if approvals already met                 |

Each scenario is implemented as an HUnit `testCase` that constructs the `ScriptContext`, calls `mkFundValidator` and asserts the boolean result.

### Serialization coverage

* Datum roundtrip: `FundDatum -> BuiltinData -> FundDatum` equality check.
* Redeemer roundtrip: checks `Approve`, `Release`, `Refund` individually.

### Artifact generation verification

* The test `testPlutusScriptGeneration` serializes the validator, writes a `.plutus` file via `writeFileTextEnvelope`, and asserts that the write returned `Right ()`. This closes the artifact generation path and provides confidence artifacts are reproducible.

### How the tests align with the CP108 assessment

* **Smart Contract Logic**: The `Validator Logic Tests` cover the approval quorum, deadline enforcement, and signer checks required by the spec.
* **Emulator Tests**: The unit tests simulate the exact same logical flows you would exercise in an emulator (signers/time windows), but in a deterministic, unit-test style that is ideal for CI and static checking.
* **Documentation & Demo**: The `main`-wrapper writes artifacts and runs the whole test suite, making it trivial to produce evidence for a demo video.

---

## Code Walkthrough (guided)

This section explains the important code blocks and where to look in `src/Main.hs`.

1. **Type definitions (Datum & Redeemer)**

   * `FundDatum` and `FundRedeemer` are defined at the top. See the `PlutusTx.unstableMakeIsData` and `PlutusTx.makeIsDataIndexed` lines — these create the serialization instances used by the validator and tests.

2. **Validator functions**

   * `mkFundValidator` is the dispatcher that pattern matches on `FundRedeemer` and delegates to `validateApprove`, `validateRelease`, or `validateRefund`.
   * Each `validate*` function extracts `txInfo` via `scriptContextTxInfo ctx` and evaluates signer/time/approval predicates.

3. **Wrapper & Compilation**

   * `wrapper` converts `BuiltinData` inputs into typed values and returns `()` on success or errors on failure.
   * `validator` is produced by `mkValidatorScript $$(compile [|| wrapper ||])` which yields a `Validator` suitable for serialization and use with `cardano-api`.

4. **Mocks & Test helpers**

   * `mockPKH1`, `mockPKH2`, ... are deterministic dummy `PubKeyHash` values used in tests.
   * `createMockContext` constructs a `ScriptContext` with a configurable signer set and validity range.

5. **Tests**

   * The `tasty` `TestTree` is assembled from several `TestTree` nodes: `testValidatorCompilation`, `testSerialization`, `testLogicValidation`, and `testPlutusScriptGeneration`.
   * `defaultMain` runs the whole `TestTree` when the binary is executed.

6. **Artifact writing**

   * `writeValidator` and `saveDatum` implement artifact serialization and file output. The `main` function runs them before executing tests.

---

## How to run & reproduce (commands)

**Prerequisites**: GHC, Cabal (or stack), and the project dependencies resolved (Plutus libs, cardano-api). Use your usual development workflow (Nix, cabal, stack) as appropriate.

* With `cabal`:

```bash
nix develop
cabal build
cabal run public-fund-exe
```

* With `stack` (if you use stack):

```bash
nix develop
stack build
stack exec public-fund-exe
```

* With `nix` / flakes: enter your dev shell and run the above cabal/stack commands.

**Notes**:

* Running the compiled executable will write `./assets/public_fund.plutus` and `./assets/sample_fund_datum.cbor` before executing the test suite.

---

## Demo Guidance (detailed)

Suggested demo steps (short video or live):

1. **Intro (30–60s)** — High-level problem statement: public funds governance, n-of-m approvals, and the anti-corruption objective.
2. **Show repo & files (30s)** — Open `src/Main.hs` and highlight `FundDatum`, `FundRedeemer`, and the three validator functions.
3. **Artifact generation (30s)** — Run the binary and show the `assets/` directory populating `public_fund.plutus` and `sample_fund_datum.cbor`.
4. **Run tests (1–2 min)** — Run the test suite and show green `tasty` output. Point to a few key test cases in the output and explain what they verify.
5. **Explain mocks & approach (1 min)** — Show `createMockContext` and how tests simulate deadlines and signers without an emulator.
6. **Wrap-up (30s)** — Restate that all logic is verifiable on-chain via the validator and that the test suite provides deterministic coverage of the core governance semantics.

Total demo length: aim for 3–5 minutes.

---

## File map (high-level)

* `src/Main.hs` — On-chain validator, wrapper, artifact helpers, mock data, and test suite (single-file test harness).
* `assets/public_fund.plutus` — (generated) compiled validator script.
* `assets/sample_fund_datum.cbor` — (generated) serialized sample datum.
