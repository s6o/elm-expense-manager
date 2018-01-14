module Manager.Account
    exposing
        ( FieldInput(..)
        , fieldInput
        , id
        , init
        , initialBalance
        )

import DRec exposing (DError, DField, DRec, DType(..))
import Dict exposing (Dict)
import FormatNumber
import Manager.Currency as Currency
import Maybe.Extra as EMaybe
import Regex


type alias Parent m =
    { m
        | accounts : Dict Int DRec
        , currency : DRec
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


fieldInput : FieldInput -> Int -> String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput action accountId field model value =
    Dict.get accountId model.accounts
        |> Maybe.map
            (\drec ->
                let
                    newDRec =
                        case field of
                            "initial_balance" ->
                                DRec.setWith field (validateBalance action model) value drec

                            _ ->
                                DRec.setString field value drec
                in
                ( { model | accounts = Dict.insert accountId newDRec model.accounts }
                , Cmd.none
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


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
                        FormatNumber.format (Currency.locale model.currency 2) amount
                   )
            )


validateBalance : FieldInput -> Parent m -> String -> Maybe DField
validateBalance action model value =
    case action of
        Collect ->
            Nothing

        Validate ->
            let
                decSep =
                    DRec.get "decimal_separator" model.currency
                        |> DRec.toString
                        |> Result.withDefault "."

                thoSep =
                    DRec.get "thousand_separator" model.currency
                        |> DRec.toString
                        |> Result.withDefault ""

                subRatio =
                    DRec.get "sub_unit_ratio" model.currency
                        |> DRec.toInt
                        |> Result.map Basics.toFloat
                        |> Result.withDefault 1.0

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
