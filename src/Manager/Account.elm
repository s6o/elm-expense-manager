module Manager.Account
    exposing
        ( Account(..)
        , FieldInput(..)
        , bankAccount
        , bankName
        , defaultId
        , empty
        , fieldInput
        , id
        , init
        , initialBalance
        , name
        , validate
        )

import DRec exposing (DError, DField, DRec, DType(..), DValue(..))
import Dict exposing (Dict)
import FormatNumber
import Manager.Currency as Currency exposing (Currency(..))
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


type alias Parent m =
    { m
        | accounts : Dict Int Account
        , currency : Currency
    }


type Account
    = Account DRec


type FieldInput
    = Collect
    | Validate


defaultId : Int
defaultId =
    0


empty : Int -> Account
empty managerId =
    init
        |> (\(Account drec) ->
                DRec.setInt "mgr_id" managerId drec
                    |> DRec.setString "name" "<name-your-account>"
                    |> DRec.setInt "initial_balance" 0
                    |> DRec.setWith "bank_account" (DRec.fromMaybe DRec.fromString >> Just) Nothing
                    |> DRec.setWith "bank_name" (DRec.fromMaybe DRec.fromString >> Just) Nothing
           )
        |> Account


init : Account
init =
    DRec.init
        |> DRec.field "pk_id" DInt
        |> DRec.field "mgr_id" DInt
        |> DRec.field "name" DString
        |> DRec.field "initial_balance" DInt
        |> DRec.field "bank_account" (DMaybe VString)
        |> DRec.field "bank_name" (DMaybe VString)
        |> Account


id : Account -> Int
id (Account drec) =
    DRec.get "pk_id" drec
        |> DRec.toInt
        |> Result.withDefault 0


name : Account -> String
name (Account drec) =
    DRec.get "name" drec
        |> DRec.toString
        |> Result.withDefault ""


initialBalance : Currency -> Account -> String
initialBalance currency (Account drec) =
    DRec.fieldBuffer "initial_balance" drec
        |> Maybe.withDefault
            (DRec.get "initial_balance" drec
                |> DRec.toInt
                |> Result.withDefault 0
                |> (\balance ->
                        let
                            amount =
                                toFloat balance / (toFloat <| Currency.subUnitRatio currency)
                        in
                        FormatNumber.format (Currency.locale currency) amount
                   )
            )


bankAccount : Account -> String
bankAccount (Account drec) =
    DRec.get "bank_account" drec
        |> DRec.toString
        |> Result.withDefault ""


bankName : Account -> String
bankName (Account drec) =
    DRec.get "bank_name" drec
        |> DRec.toString
        |> Result.withDefault ""


fieldInput : FieldInput -> Int -> String -> Parent m -> String -> ( Parent m, Cmd msg )
fieldInput action accountId field model value =
    Dict.get accountId model.accounts
        |> Maybe.map
            (\(Account drec) ->
                let
                    newDRec =
                        update action field model.currency drec value
                in
                ( { model | accounts = Dict.insert accountId (Account newDRec) model.accounts }
                , Cmd.none
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


update : FieldInput -> String -> Currency -> DRec -> String -> DRec
update action field currency account value =
    case field of
        "initial_balance" ->
            Currency.validateNumerics currency value
                |> (\v ->
                        DRec.setWith field (validateBalance action currency) v account
                   )

        "bank_account" ->
            let
                mv =
                    if not <| String.isEmpty value then
                        Just value
                    else
                        Nothing
            in
            DRec.setWith field (DRec.fromMaybe DRec.fromString >> Just) mv account

        "bank_name" ->
            let
                mv =
                    if not <| String.isEmpty value then
                        Just value
                    else
                        Nothing
            in
            DRec.setWith field (DRec.fromMaybe DRec.fromString >> Just) mv account

        _ ->
            DRec.setString field value account


validateBalance : FieldInput -> Currency -> String -> Maybe DField
validateBalance action currency value =
    case action of
        Collect ->
            Nothing

        Validate ->
            Currency.validateAmount currency value
                |> Maybe.map DRec.fromInt


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
            (\(Account drec) ->
                let
                    nameLen =
                        name (Account drec) |> String.length

                    required =
                        DRec.fieldNames drec
                            -- drop pk_id, is handled by back-end
                            |> List.drop 1

                    -- make sure all partial input are validated as onBlur might
                    -- not be always triggered
                    storeDRec =
                        required
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
                if DRec.isValidWith required storeDRec && nameLen > 0 then
                    Meld.init
                        { model
                            | accounts =
                                Dict.insert
                                    (id (Account drec))
                                    (Account storeDRec)
                                    model.accounts
                        }
                        |> Task.succeed
                else
                    fail "Correct account field errors."
            )
        |> Maybe.withDefault (fail <| "Incorrect account id: " ++ Basics.toString accountId)
