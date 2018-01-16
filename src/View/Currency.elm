module View.Currency exposing (view)

import Api.Currency
import DRec
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Currency as Currency
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))
import Model exposing (Model)


view : Model -> Html Msg
view model =
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
                    |> Options.when (String.length (Currency.isoCode model.currency) /= 3)
                , css "width" "100%"
                , Currency.isoCode model.currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput "iso_code") model
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
                    |> Options.when (Currency.subUnitRatio model.currency <= 0)
                , css "width" "100%"
                , Currency.subUnitRatio model.currency
                    |> Basics.toString
                    |> Textfield.value
                , TextInput (Currency.fieldInput "sub_unit_ratio") model
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
                , Currency.symbol model.currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput "symbol") model
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
                , Currency.decimalSeparator model.currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput "decimal_separator") model
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
                , Currency.thousandSeparator model.currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput "thousand_separator") model
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
