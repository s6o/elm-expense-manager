module Manager.Account
    exposing
        ( Account(..)
        , AccountField(..)
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
    = Account (DRec AccountField)


type AccountField
    = PkId
    | MgrId
    | Name
    | InitialBalance
    | BankAccount
    | BankName


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
                DRec.setInt MgrId managerId drec
                    |> DRec.setString Name "<name-your-account>"
                    |> DRec.setInt InitialBalance 0
                    |> DRec.setWith BankAccount (DRec.fromMaybe DRec.fromString >> Just) Nothing
                    |> DRec.setWith BankName (DRec.fromMaybe DRec.fromString >> Just) Nothing
           )
        |> Account


init : Account
init =
    DRec.init
        |> DRec.field PkId DInt
        |> DRec.field MgrId DInt
        |> DRec.field Name DString
        |> DRec.field InitialBalance DInt
        |> DRec.field BankAccount (DMaybe VString)
        |> DRec.field BankName (DMaybe VString)
        |> Account


id : Account -> Int
id (Account drec) =
    DRec.get PkId drec
        |> DRec.toInt
        |> Result.withDefault 0


name : Account -> String
name (Account drec) =
    DRec.get Name drec
        |> DRec.toString
        |> Result.withDefault ""


initialBalance : Currency -> Account -> String
initialBalance currency (Account drec) =
    DRec.fieldBuffer InitialBalance drec
        |> Maybe.withDefault
            (DRec.get InitialBalance drec
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
    DRec.get BankAccount drec
        |> DRec.toString
        |> Result.withDefault ""


bankName : Account -> String
bankName (Account drec) =
    DRec.get BankName drec
        |> DRec.toString
        |> Result.withDefault ""


fieldInput : FieldInput -> Int -> AccountField -> String -> Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
fieldInput action accountId field value meld =
    let
        model =
            Meld.model meld
    in
    Dict.get accountId model.accounts
        |> Maybe.map
            (\(Account drec) ->
                let
                    newDRec =
                        update action field model.currency drec value

                    taskModel ma =
                        { ma | accounts = Dict.insert accountId (Account newDRec) ma.accounts }
                in
                Meld.withMerge taskModel meld
                    |> Task.succeed
            )
        |> Maybe.withDefault (Task.succeed meld)


update : FieldInput -> AccountField -> Currency -> DRec AccountField -> String -> DRec AccountField
update action field currency account value =
    case field of
        InitialBalance ->
            Currency.validateNumerics currency value
                |> (\v ->
                        DRec.setWith field (validateBalance action currency) v account
                   )

        BankAccount ->
            let
                mv =
                    if not <| String.isEmpty value then
                        Just value
                    else
                        Nothing
            in
            DRec.setWith field (DRec.fromMaybe DRec.fromString >> Just) mv account

        BankName ->
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


validateBalance : FieldInput -> Currency -> String -> Maybe (DField a)
validateBalance action currency value =
    case action of
        Collect ->
            Nothing

        Validate ->
            Currency.validateAmount currency value
                |> Maybe.map DRec.fromInt


validate : Int -> Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
validate accountId meld =
    let
        model =
            Meld.model meld

        fail msg =
            msg
                |> EMsg model
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
