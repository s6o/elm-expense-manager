module View.Currency exposing (view)

import Api.Currency
import DRec
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Currency as MCurrency
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
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
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "ISO Code"
                , Textfield.autofocus
                , Textfield.floatingLabel
                , Textfield.maxlength 3
                , Textfield.error "Required, 3 characters"
                    |> Options.when (String.length isoCode /= 3)
                , css "width" "100%"
                , isoCode
                    |> Textfield.value
                , TextInput (MCurrency.fieldInput "iso_code") model
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Sub Unit Ratio"
                , Textfield.floatingLabel
                , Textfield.error "Required, an integer value > 0"
                    |> Options.when (subUnitRatio <= 0)
                , css "width" "100%"
                , subUnitRatio
                    |> Basics.toString
                    |> Textfield.value
                , TextInput (MCurrency.fieldInput "sub_unit_ratio") model
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 2 ]
                model.mdl
                [ Textfield.label "Symbol"
                , Textfield.floatingLabel
                , Textfield.maxlength 3
                , css "width" "100%"
                , DRec.get "symbol" model.currencyMgr
                    |> DRec.toString
                    |> Result.withDefault ""
                    |> Textfield.value
                , TextInput (MCurrency.fieldInput "symbol") model
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 3 ]
                model.mdl
                [ Textfield.label "Decimal Separator"
                , Textfield.floatingLabel
                , Textfield.maxlength 1
                , css "width" "100%"
                , DRec.get "decimal_separator" model.currencyMgr
                    |> DRec.toString
                    |> Result.withDefault ""
                    |> Textfield.value
                , TextInput (MCurrency.fieldInput "decimal_separator") model
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 4 ]
                model.mdl
                [ Textfield.label "Thousand Separator"
                , Textfield.floatingLabel
                , Textfield.maxlength 1
                , css "width" "100%"
                , DRec.get "thousand_separator" model.currencyMgr
                    |> DRec.toString
                    |> Result.withDefault ""
                    |> Textfield.value
                , TextInput (MCurrency.fieldInput "thousand_separator") model
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ 5 ]
                model.mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <| Request [ Api.Currency.save ]
                ]
                [ text "Save" ]
            ]
        ]
