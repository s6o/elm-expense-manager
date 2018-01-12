module Manager.Currency
    exposing
        ( fieldInput
        , init
        , locale
        , subUnitRatio
        , validate
        )

import DRec exposing (DError, DRec, DType(..))
import FormatNumber.Locales exposing (Locale)
import Meld exposing (Error(..), Meld)
import String
import Task exposing (Task)


type alias Parent m =
    { m | currency : DRec }


init : DRec
init =
    DRec.empty
        |> DRec.field "iso_code" DString
        |> DRec.field "sub_unit_ratio" DInt
        |> DRec.field "symbol" DString
        |> DRec.field "decimal_separator" DString
        |> DRec.field "thousand_separator" DString


fieldInput : String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput field model value =
    case field of
        "sub_unit_ratio" ->
            String.toInt value
                |> Result.map
                    (\i ->
                        ( { model | currency = DRec.setInt field i model.currency }
                        , Cmd.none
                        )
                    )
                |> Result.withDefault ( model, Cmd.none )

        _ ->
            ( { model | currency = DRec.setString field value model.currency }
            , Cmd.none
            )


locale : DRec -> Int -> Locale
locale drec decPlaces =
    { decimals = decPlaces
    , thousandSeparator =
        DRec.get "thousand_separator" drec
            |> DRec.toString
            |> Result.withDefault ""
    , decimalSeparator =
        DRec.get "decimal_separator" drec
            |> DRec.toString
            |> Result.withDefault ""
    , negativePrefix = "-"
    , negativeSuffix = ""
    }


subUnitRatio : DRec -> Int
subUnitRatio drec =
    DRec.get "sub_unit_ratio" drec
        |> DRec.toInt
        |> Result.withDefault 1


validate : Meld (Parent m) Error msg -> Task Error (Meld (Parent m) Error msg)
validate meld =
    let
        model =
            Meld.model meld

        fail msg =
            msg
                |> EMsg
                |> Task.fail
    in
    if DRec.isEmpty model.currency then
        fail "Currency fields not set: ISO Code and Sub Unit Ratio are mandatory."
    else
        let
            isoCode =
                DRec.get "iso_code" model.currency
                    |> DRec.toString
                    |> Result.withDefault ""

            subUnitRatio =
                DRec.get "sub_unit_ratio" model.currency
                    |> DRec.toInt
                    |> Result.withDefault 0
        in
        if String.length isoCode == 3 && subUnitRatio > 0 then
            Task.succeed meld
        else
            fail "Required fields: ISO Code, Sub Unit Ratio"
