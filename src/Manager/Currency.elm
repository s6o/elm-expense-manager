module Manager.Currency
    exposing
        ( fieldInput
        , init
        , validate
        )

import DRec exposing (DError, DRec, DType(..))
import Meld exposing (Error(..), Meld)
import String
import Task exposing (Task)


type alias Parent m =
    { m | currencyMgr : Result DError DRec }


init : Result DError DRec
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
                        DRec.setInt field i model.currencyMgr
                            |> Result.map (\drec -> ( { model | currencyMgr = Ok drec }, Cmd.none ))
                            |> Result.withDefault ( model, Cmd.none )
                    )
                |> Result.withDefault ( model, Cmd.none )

        _ ->
            DRec.setString field value model.currencyMgr
                |> Result.map (\drec -> ( { model | currencyMgr = Ok drec }, Cmd.none ))
                |> Result.withDefault ( model, Cmd.none )


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
    if DRec.isEmpty model.currencyMgr then
        fail "Currency fields not set: ISO Code and Sub Unit Ratio are mandatory."
    else
        let
            isoCode =
                DRec.get "iso_code" model.currencyMgr
                    |> DRec.toString
                    |> Result.withDefault ""

            subUnitRatio =
                DRec.get "sub_unit_ratio" model.currencyMgr
                    |> DRec.toInt
                    |> Result.withDefault 0
        in
        if String.length isoCode == 3 && subUnitRatio > 0 then
            Task.succeed meld
        else
            fail "Required fields: ISO Code, Sub Unit Ratio"
