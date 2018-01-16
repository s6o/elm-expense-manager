module Manager.Account
    exposing
        ( FieldInput(..)
        , fieldInput
        , id
        , init
        , initialBalance
        , name
        , validate
        )

import DRec exposing (DError, DField, DRec, DType(..))
import Dict exposing (Dict)
import FormatNumber
import Manager.Currency as Currency exposing (Currency)
import Maybe.Extra as EMaybe
import Meld exposing (Error(..), Meld)
import Regex
import Task exposing (Task)


type alias Parent m =
    { m
        | accounts : Dict Int DRec
        , currency : Currency
    }


type FieldInput
    = Collect
    | Validate


init : DRec
init =
    DRec.init
        |> DRec.field "aid" DInt
        |> DRec.field "mgr_id" DInt
        |> DRec.field "name" DString
        |> DRec.field "initial_balance" DInt
        |> DRec.field "bank_account" DString
        |> DRec.field "bank_name" DString


id : DRec -> Int
id drec =
    DRec.get "aid" drec
        |> DRec.toInt
        |> Result.withDefault 0


name : DRec -> String
name drec =
    DRec.get "name" drec
        |> DRec.toString
        |> Result.withDefault ""


fieldInput : FieldInput -> Int -> String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput action accountId field model value =
    Dict.get accountId model.accounts
        |> Maybe.map
            (\drec ->
                let
                    newDRec =
                        update action field model.currency drec value
                in
                ( { model | accounts = Dict.insert accountId newDRec model.accounts }
                , Cmd.none
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


update : FieldInput -> String -> Currency -> DRec -> String -> DRec
update action field currency account value =
    case field of
        "initial_balance" ->
            DRec.setWith field (validateBalance action currency) value account

        _ ->
            DRec.setString field value account


initialBalance : Parent m -> DRec -> String
initialBalance model drec =
    DRec.fieldBuffer "initial_balance" drec
        |> Maybe.withDefault
            (DRec.get "initial_balance" drec
                |> DRec.toInt
                |> Result.withDefault 0
                |> (\balance ->
                        let
                            amount =
                                toFloat balance / (toFloat <| Currency.subUnitRatio model.currency)
                        in
                        FormatNumber.format (Currency.locale model.currency) amount
                   )
            )


validateBalance : FieldInput -> Currency -> String -> Maybe DField
validateBalance action currency value =
    case action of
        Collect ->
            Nothing

        Validate ->
            let
                decSep =
                    Currency.decimalSeparator currency

                thoSep =
                    Currency.thousandSeparator currency

                subRatio =
                    Currency.subUnitRatio currency
                        |> Basics.toFloat

                numRe =
                    "^\\s*-?\\d+(\\" ++ decSep ++ "\\d{1,2})?\\s*$"
            in
            Regex.replace Regex.All (Regex.regex thoSep) (\_ -> "") value
                |> Regex.find (Regex.AtMost 1) (Regex.regex numRe)
                |> List.head
                |> Maybe.map
                    (\r ->
                        String.toFloat r.match
                            |> Result.map (\f -> Basics.round (f * subRatio) |> DRec.fromInt)
                            |> Result.toMaybe
                    )
                |> EMaybe.join


validate : Int -> Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
validate accountId meld =
    let
        model =
            Meld.model meld

        fail msg =
            msg
                |> EMsg
                |> Task.fail
    in
    Dict.get accountId model.accounts
        |> Maybe.map
            (\drec ->
                let
                    nameLen =
                        name drec |> String.length

                    -- make sure all partial input are validated as onBlur might not be always triggered
                    storeDRec =
                        DRec.fieldNames drec
                            |> List.filter
                                (\fn ->
                                    DRec.fieldBuffer fn drec
                                        |> Maybe.map (\_ -> True)
                                        |> Maybe.withDefault False
                                )
                            |> List.foldl
                                (\fn accum ->
                                    DRec.fieldBuffer fn accum
                                        |> Maybe.map (update Validate fn model.currency accum)
                                        |> Maybe.withDefault accum
                                )
                                drec
                in
                if DRec.isValid storeDRec && nameLen > 0 then
                    Meld.init { model | accounts = Dict.insert (id drec) storeDRec model.accounts }
                        |> Task.succeed
                else
                    fail "Correct account field errors."
            )
        |> Maybe.withDefault (fail <| "Incorrect account id: " ++ Basics.toString accountId)
