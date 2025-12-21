{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module PublicFund where

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import Cardano.Api (writeFileTextEnvelope, displayError)
import Cardano.Api.Shelley
    ( PlutusScript (..)
    , PlutusScriptV2
    )

import PlutusTx
import PlutusTx.Prelude hiding (($), (<>), Show, find)
import Prelude (IO, FilePath, print, putStrLn, Show, ($), (<>))

import Plutus.V2.Ledger.Api
    ( Validator
    , ScriptContext(..)
    , TxInfo(..)
    , TxOut(..)
    , Datum(..)
    , Value
    , PubKeyHash
    , POSIXTime
    , ScriptPurpose(..)
    , mkValidatorScript
    , OutputDatum (..)
    , unValidatorScript
    , TxOutRef
    )
import Plutus.V2.Ledger.Contexts
    ( txSignedBy
    , valuePaidTo
    , getContinuingOutputs
    , txInInfoOutRef
    , txInInfoResolved
    , ownHash
    , findOwnInput
    , findDatum
    )
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value    as Value
import Plutus.V1.Ledger.Address ( Address(..))
import Plutus.V1.Ledger.Credential( Credential(..))

-- -- Import your types
-- import PublicFund.Types

--------------------------------------------------------------------------------
-- DATUM
--------------------------------------------------------------------------------
data FundDatum = FundDatum
    { officials         :: [PubKeyHash]
    , requiredApprovals :: Integer
    , deadline          :: POSIXTime
    , depositor         :: PubKeyHash
    , recipient         :: PubKeyHash        -- who gets the funds on release
    , approvalsCount    :: Integer           -- previously tracked approvals
    }
    deriving Show

unstableMakeIsData ''FundDatum

--------------------------------------------------------------------------------
-- REDEEMER
--------------------------------------------------------------------------------
data FundRedeemer
    = Approve          -- Just approve, keep funds in script
    | Release          -- Release funds to recipient
    | Refund           -- Refund to depositor
    deriving Show

PlutusTx.makeIsDataIndexed ''FundRedeemer
    [ ('Approve, 0)
    , ('Release, 1)
    , ('Refund,  2)
    ]

--------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------
{-# INLINABLE mkFundValidator #-}
mkFundValidator :: FundDatum -> FundRedeemer -> ScriptContext -> Bool
mkFundValidator dat red ctx =
    case red of
        Approve  -> validateApprove dat ctx
        Release  -> validateRelease dat ctx
        Refund   -> validateRefund dat ctx

--------------------------------------------------------------------------------
-- APPROVE (just increment approval count)
--------------------------------------------------------------------------------
{-# INLINABLE validateApprove #-}
validateApprove :: FundDatum -> ScriptContext -> Bool
validateApprove FundDatum{officials, requiredApprovals, deadline, approvalsCount} ctx =
    traceIfFalse "Deadline passed" beforeDeadline &&
    traceIfFalse "Not enough valid approvals for final approval" (approvalsCount + 1 < requiredApprovals) &&
    traceIfFalse "Not an official" isOfficial &&
    traceIfFalse "Funds must stay in script" fundsStayInScript
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        Interval.to deadline `Interval.contains` txInfoValidRange info

    signers :: [PubKeyHash]
    signers = txInfoSignatories info

    -- Check if signer is an official
    isOfficial :: Bool
    isOfficial = any (`elem` officials) signers

    -- For approval, funds must stay in script with updated datum
    fundsStayInScript :: Bool
    fundsStayInScript =
        let ownValHash = ownHash ctx
            outputs = getContinuingOutputs ctx
        in case outputs of
            [out] -> 
                let outValHash = case txOutAddress out of
                        Address (ScriptCredential vh) _ -> vh == ownValHash
                        _ -> False
                in outValHash && txOutValue out == scriptInputValue ctx
            _ -> False

--------------------------------------------------------------------------------
-- RELEASE (send funds to recipient)
--------------------------------------------------------------------------------
{-# INLINABLE validateRelease #-}
validateRelease :: FundDatum -> ScriptContext -> Bool
validateRelease FundDatum{officials, requiredApprovals, deadline, recipient, approvalsCount} ctx =
    traceIfFalse "Deadline passed" beforeDeadline &&
    traceIfFalse "Not enough approvals" (approvalsCount >= requiredApprovals) &&
    traceIfFalse "Recipient not paid" recipientPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    beforeDeadline :: Bool
    beforeDeadline =
        Interval.to deadline `Interval.contains` txInfoValidRange info

    recipientPaid :: Bool
    recipientPaid =
        let paid = valuePaidTo info recipient
        in Value.geq paid (scriptInputValue ctx)

--------------------------------------------------------------------------------
-- REFUND (back to depositor)
--------------------------------------------------------------------------------
{-# INLINABLE validateRefund #-}
validateRefund :: FundDatum -> ScriptContext -> Bool
validateRefund FundDatum{deadline, depositor, approvalsCount, requiredApprovals} ctx =
    traceIfFalse "Deadline not reached" afterDeadline &&
    traceIfFalse "Depositor did not sign" depositorSigned &&
    traceIfFalse "Funds not refunded" refunded &&
    traceIfFalse "Cannot refund if enough approvals" (approvalsCount < requiredApprovals)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    afterDeadline :: Bool
    afterDeadline =
        Interval.from deadline `Interval.contains` txInfoValidRange info

    depositorSigned :: Bool
    depositorSigned = txSignedBy info depositor

    refunded :: Bool
    refunded =
        let paid = valuePaidTo info depositor
        in Value.geq paid (scriptInputValue ctx)

--------------------------------------------------------------------------------
-- SCRIPT INPUT VALUE
--------------------------------------------------------------------------------
{-# INLINABLE scriptInputValue #-}
scriptInputValue :: ScriptContext -> Value
scriptInputValue ctx =
    case findOwnInput ctx of
        Just i  -> txOutValue (txInInfoResolved i)
        Nothing -> traceError "Script input missing"

--------------------------------------------------------------------------------
-- WRAPPER
--------------------------------------------------------------------------------
{-# INLINABLE wrapper #-}
wrapper :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapper d r c =
    if mkFundValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapper ||])

--------------------------------------------------------------------------------
-- WRITE SCRIPT
--------------------------------------------------------------------------------
writeValidator :: FilePath -> Validator -> IO ()
writeValidator file val = do
    let script = unValidatorScript val
        bs     = serialise script
        sh     = SBS.toShort (LBS.toStrict bs)
        scr    = PlutusScriptSerialised sh :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope file Nothing scr
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote script to: " <> file)

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Compiled Public Fund Release (Anti-Corruption) contract!"
    writeValidator "./assets/public_fund.plutus" validator