module Manager.Currency
    exposing
        ( Currency(..)
        , CurrencyField(..)
        , decimalSeparator
        , fieldInput
        , init
        , isoCode
        , locale
        , subUnitRatio
        , symbol
        , thousandSeparator
        , validate
        , validateAmount
        , validateNumerics
        )

import DRec exposing (DError, DRec, DType(..))
import FormatNumber.Locales exposing (Locale)
import Maybe.Extra as EMaybe
import Meld exposing (Error(..), Meld)
import Regex
import String
import Task exposing (Task)


type alias Parent m =
    { m | currency : Currency }


type Currency
    = Currency (DRec CurrencyField)


type CurrencyField
    = IsoCode
    | SubUnitRatio
    | Symbol
    | DecimalSeparator
    | ThousandSeparator


init : Currency
init =
    DRec.init
        |> DRec.field IsoCode DString
        |> DRec.field SubUnitRatio DInt
        |> DRec.field Symbol DString
        |> DRec.field DecimalSeparator DString
        |> DRec.field ThousandSeparator DString
        |> Currency


isoCode : Currency -> String
isoCode (Currency drec) =
    DRec.get IsoCode drec
        |> DRec.toString
        |> Result.withDefault ""


subUnitRatio : Currency -> Int
subUnitRatio (Currency drec) =
    DRec.get SubUnitRatio drec
        |> DRec.toInt
        |> Result.withDefault 1


symbol : Currency -> String
symbol (Currency drec) =
    DRec.get Symbol drec
        |> DRec.toString
        |> Result.withDefault ""


decimalSeparator : Currency -> String
decimalSeparator (Currency drec) =
    DRec.get DecimalSeparator drec
        |> DRec.toString
        |> Result.withDefault ""


thousandSeparator : Currency -> String
thousandSeparator (Currency drec) =
    DRec.get ThousandSeparator drec
        |> DRec.toString
        |> Result.withDefault ""


fieldInput : CurrencyField -> String -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
fieldInput field value meld =
    let
        model =
            Meld.model meld

        (Currency drec) =
            model.currency
    in
    Task.succeed <|
        case field of
            SubUnitRatio ->
                String.toInt value
                    |> Result.map
                        (\i ->
                            let
                                taskModel ma =
                                    { ma | currency = DRec.setInt field i drec |> Currency }
                            in
                            Meld.withMerge taskModel meld
                        )
                    |> Result.withDefault meld

            _ ->
                let
                    taskModel ma =
                        { ma | currency = DRec.setString field value drec |> Currency }
                in
                Meld.withMerge taskModel meld


locale : Currency -> Locale
locale currency =
    { decimals = 2
    , thousandSeparator = thousandSeparator currency
    , decimalSeparator = decimalSeparator currency
    , negativePrefix = "-"
    , negativeSuffix = ""
    }


validateAmount : Currency -> String -> Maybe Int
validateAmount currency value =
    let
        decSep =
            decimalSeparator currency
                |> (\ds ->
                        if ds == "." then
                            "\\" ++ ds
                        else
                            ds
                   )

        thoSep =
            thousandSeparator currency
                |> (\ts ->
                        if ts == "." then
                            "\\" ++ ts
                        else
                            ts
                   )

        subRatio =
            subUnitRatio currency
                |> Basics.toFloat

        numRe =
            "^\\s*-?\\d+(" ++ decSep ++ "\\d{1,2})?\\s*$"
    in
    Regex.replace Regex.All (Regex.regex thoSep) (\_ -> "") value
        |> Regex.find (Regex.AtMost 1) (Regex.regex numRe)
        |> List.head
        |> Maybe.map
            (\r ->
                String.toFloat r.match
                    |> Result.map (\f -> Basics.round (f * subRatio))
                    |> Result.toMaybe
            )
        |> EMaybe.join


validateNumerics : Currency -> String -> String
validateNumerics currency value =
    let
        decSep =
            decimalSeparator currency
                |> (\ds ->
                        if ds == "." then
                            "\\" ++ ds
                        else
                            ds
                   )

        thoSep =
            thousandSeparator currency
                |> (\ts ->
                        if ts == "." then
                            "\\" ++ ts
                        else
                            ts
                   )

        numRe =
            "[^0-9" ++ decSep ++ thoSep ++ "]"
    in
    Regex.replace Regex.All (Regex.regex numRe) (\_ -> "") value


validate : Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
validate meld =
    let
        model =
            Meld.model meld

        (Currency drec) =
            model.currency

        fail msg =
            msg
                |> EMsg
                |> Task.fail
    in
    if DRec.isEmpty drec then
        fail "Currency fields not set: ISO Code and Sub Unit Ratio are mandatory."
    else if
        String.length (isoCode model.currency)
            == 3
            && subUnitRatio model.currency
            > 0
    then
        Task.succeed meld
    else
        fail "Required fields: ISO Code, Sub Unit Ratio"
