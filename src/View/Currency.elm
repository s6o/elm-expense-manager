module View.Currency exposing (view)

import Api.Currency
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Currency as Currency exposing (Currency)
import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))


view : Material.Model -> Currency -> Html Msg
view mdl currency =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ 0 ]
                mdl
                [ Textfield.label "ISO Code"
                , Textfield.autofocus
                , Textfield.floatingLabel
                , Textfield.maxlength 3
                , Textfield.error "Required, 3 characters"
                    |> Options.when (String.length (Currency.isoCode currency) /= 3)
                , css "width" "100%"
                , Currency.isoCode currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput Currency.IsoCode)
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 1 ]
                mdl
                [ Textfield.label "Sub Unit Ratio"
                , Textfield.floatingLabel
                , Textfield.error "Required, an integer value > 0"
                    |> Options.when (Currency.subUnitRatio currency <= 0)
                , css "width" "100%"
                , Currency.subUnitRatio currency
                    |> Basics.toString
                    |> Textfield.value
                , TextInput (Currency.fieldInput Currency.SubUnitRatio)
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 2 ]
                mdl
                [ Textfield.label "Symbol"
                , Textfield.floatingLabel
                , Textfield.maxlength 3
                , css "width" "100%"
                , Currency.symbol currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput Currency.Symbol)
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 3 ]
                mdl
                [ Textfield.label "Decimal Separator"
                , Textfield.floatingLabel
                , Textfield.maxlength 1
                , css "width" "100%"
                , Currency.decimalSeparator currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput Currency.DecimalSeparator)
                    |> Options.onInput
                , KeyEvent.onEnter <| Request [ Api.Currency.save ]
                ]
                []
            ]
        , Options.div
            []
            [ Textfield.render Mdl
                [ 4 ]
                mdl
                [ Textfield.label "Thousand Separator"
                , Textfield.floatingLabel
                , Textfield.maxlength 1
                , css "width" "100%"
                , Currency.thousandSeparator currency
                    |> Textfield.value
                , TextInput (Currency.fieldInput Currency.ThousandSeparator)
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
                mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <| Request [ Api.Currency.save ]
                ]
                [ text "Save" ]
            ]
        ]
