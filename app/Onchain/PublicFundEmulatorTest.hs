{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module PublicFundEmulatorTest where

import Control.Monad (void)
import Ledger (PubKeyHash, pubKeyHash)
import Ledger.TimeSlot (slotToEndPOSIXTime)
import Wallet.Emulator.Wallet
import Plutus.Trace.Emulator as Emulator
import Plutus.Contract
import Control.Monad.Freer.Extras as Extras

import PublicFund -- your on-chain module

-- =======================================================
-- Wallets
-- =======================================================
w1, w2, w3, w4 :: Wallet
w1 = knownWallet 1 -- Depositor
w2 = knownWallet 2 -- Beneficiary
w3 = knownWallet 3 -- Official 1
w4 = knownWallet 4 -- Official 2

-- =======================================================
-- Example Datum
-- =======================================================
datumExample :: EscrowDatum
datumExample = EscrowDatum
    { edDepositor   = pubKeyHash $ walletPubKey w1
    , edBeneficiary = pubKeyHash $ walletPubKey w2
    , edOfficials   = [pubKeyHash $ walletPubKey w3, pubKeyHash $ walletPubKey w4]
    , edApprovals   = []
    , edRequired    = 2
    , edDeadline    = slotToEndPOSIXTime def 20
    }

-- =======================================================
-- Mock endpoints for the demo
-- =======================================================
type PublicFundSchema =
        Endpoint "approve" EscrowDatum
    .\/ Endpoint "release" EscrowDatum
    .\/ Endpoint "refund"  EscrowDatum

someContract :: Contract () PublicFundSchema Text ()
someContract = selectList [approveEndpoint, releaseEndpoint, refundEndpoint] >> someContract
  where
    approveEndpoint = endpoint @"approve" $ \d -> Extras.logInfo $ "Approve called: " <> show d
    releaseEndpoint = endpoint @"release" $ \d -> Extras.logInfo $ "Release called: " <> show d
    refundEndpoint  = endpoint @"refund"  $ \d -> Extras.logInfo $ "Refund called: "  <> show d

-- =======================================================
-- Traces
-- =======================================================
-- 1️⃣ Release (success)
releaseTrace :: EmulatorTrace ()
releaseTrace = do
    Extras.logInfo @String "=== RELEASE TRACE ==="
    h1 <- activateContractWallet w1 someContract
    h2 <- activateContractWallet w2 someContract
    h3 <- activateContractWallet w3 someContract
    h4 <- activateContractWallet w4 someContract

    callEndpoint @"approve" h3 datumExample
    callEndpoint @"approve" h4 datumExample
    callEndpoint @"release" h2 datumExample

    void $ Emulator.waitNSlots 2

-- 2️⃣ Refund (deadline passed, insufficient approvals)
refundTrace :: EmulatorTrace ()
refundTrace = do
    Extras.logInfo @String "=== REFUND TRACE ==="
    h1 <- activateContractWallet w1 someContract
    h3 <- activateContractWallet w3 someContract

    callEndpoint @"approve" h3 datumExample
    void $ Emulator.waitUntilSlot 21
    callEndpoint @"refund" h1 datumExample

-- 3️⃣ Failure (attempted release before quorum)
failureTrace :: EmulatorTrace ()
failureTrace = do
    Extras.logInfo @String "=== FAILURE TRACE ==="
    h1 <- activateContractWallet w1 someContract
    h2 <- activateContractWallet w2 someContract
    h3 <- activateContractWallet w3 someContract

    callEndpoint @"approve" h3 datumExample
    callEndpoint @"release" h2 datumExample

-- =======================================================
-- Run all tests
-- =======================================================
runAllTraces :: IO ()
runAllTraces = do
    putStrLn "\n--- Running RELEASE Trace ---"
    runEmulatorTraceIO releaseTrace
    putStrLn "\n--- Running REFUND Trace ---"
    runEmulatorTraceIO refundTrace
    putStrLn "\n--- Running FAILURE Trace ---"
    runEmulatorTraceIO failureTrace
