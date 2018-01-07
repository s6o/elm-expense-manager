module View.Accounts
    exposing
        ( view
        )

import Api.Account
import DRec exposing (DError, DRec)
import Dict
import FormatNumber
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Account as Account
import Manager.Currency as Currency
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
    let
        -- 4 fields + 2 buttons: delete, save
        accountFields =
            4 + 2
    in
    div
        []
        (model.accounts
            |> Dict.values
            |> List.indexedMap (\i rr -> account model (i * accountFields) rr)
        )


account : Model -> Int -> Result DError DRec -> Html Msg
account model baseIndex drec =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ baseIndex + 0 ]
                model.mdl
                [ Textfield.label "Name"
                , Textfield.floatingLabel
                , css "width" "100%"
                , DRec.get "name" drec
                    |> DRec.toString
                    |> Result.withDefault ""
                    |> Textfield.value
                , TextInput (Account.fieldInput (Account.id drec) "name") model
                    |> Options.onInput
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ baseIndex + 1 ]
                model.mdl
                [ Textfield.label "Initial balance"
                , Textfield.floatingLabel
                , css "width" "100%"
                , DRec.get "initial_balance" drec
                    |> DRec.toInt
                    |> Result.withDefault 0
                    |> (\balance ->
                            let
                                amount =
                                    toFloat balance / Currency.subUnitRatio model.currency
                            in
                            FormatNumber.format (Currency.locale model.currency) amount
                       )
                    |> Textfield.value
                , TextInput (Account.fieldInput (Account.id drec) "initial_balance") model
                    |> Options.onInput
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ baseIndex + 2 ]
                model.mdl
                [ Textfield.label "Bank account number"
                , Textfield.floatingLabel
                , css "width" "100%"
                , DRec.get "bank_account" drec
                    |> DRec.toString
                    |> Result.withDefault ""
                    |> Textfield.value
                , TextInput (Account.fieldInput (Account.id drec) "bank_account") model
                    |> Options.onInput
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ baseIndex + 3 ]
                model.mdl
                [ Textfield.label "Bank name"
                , Textfield.floatingLabel
                , css "width" "100%"
                , DRec.get "bank_name" drec
                    |> DRec.toString
                    |> Result.withDefault ""
                    |> Textfield.value
                , TextInput (Account.fieldInput (Account.id drec) "bank_name") model
                    |> Options.onInput
                ]
                []
            ]
        , Options.div
            [ css "min-height" "50px"
            ]
            [ Options.div
                [ css "text-align" "right"
                , css "padding-bottom" "10px"
                , css "float" "left"
                ]
                [ Button.render Mdl
                    [ baseIndex + 4 ]
                    model.mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple
                    ]
                    [ text "Remove" ]
                ]
            , Options.div
                [ css "text-align" "right"
                , css "padding-bottom" "10px"
                , css "float" "right"
                ]
                [ Button.render Mdl
                    [ baseIndex + 5 ]
                    model.mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


addAccount : Model -> Int -> Html Msg
addAccount model ctrlIdx =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ ctrlIdx + 1 ]
                model.mdl
                [ Textfield.label "Name"
                , Textfield.floatingLabel
                , css "width" "100%"
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ ctrlIdx + 2 ]
                model.mdl
                [ Textfield.label "Initial balance"
                , Textfield.floatingLabel
                , css "width" "100%"
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ ctrlIdx + 3 ]
                model.mdl
                [ Textfield.label "Bank account number"
                , Textfield.floatingLabel
                , css "width" "100%"
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ ctrlIdx + 4 ]
                model.mdl
                [ Textfield.label "Bank name"
                , Textfield.floatingLabel
                , css "width" "100%"
                ]
                []
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ ctrlIdx + 5 ]
                model.mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                ]
                [ text "Add" ]
            ]
        ]
