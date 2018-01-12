module Manager.Account
    exposing
        ( fieldInput
        , id
        , init
        )

import DRec exposing (DError, DRec, DType(..))
import Dict exposing (Dict)
import Regex


type alias Parent m =
    { m
        | accounts : Dict Int DRec
        , currency : DRec
    }


init : DRec
init =
    DRec.empty
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


fieldInput : Int -> String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput accountId field model value =
    Dict.get accountId model.accounts
        |> Maybe.map
            (\drec ->
                let
                    newDRec =
                        case field of
                            "initial_balance" ->
                                validateBalance model drec value

                            _ ->
                                DRec.setString field value drec
                in
                ( { model | accounts = Dict.insert accountId newDRec model.accounts }
                , Cmd.none
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


validateBalance : Parent m -> DRec -> String -> DRec
validateBalance model drec value =
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
            "^\\s*-?\\d+(\\" ++ decSep ++ ")?(\\d{1,2})?\\s*$"
    in
    Regex.replace Regex.All (Regex.regex thoSep) (\_ -> "") value
        |> Regex.find (Regex.AtMost 1) (Regex.regex numRe)
        |> List.head
        |> Maybe.map
            (\r ->
                String.toFloat r.match
                    |> Result.map
                        (\f -> DRec.setInt "initial_balance" (Basics.round (f * subRatio)) drec)
                    |> Result.withDefault drec
            )
        |> Maybe.withDefault drec
