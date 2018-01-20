module View.Accounts
    exposing
        ( view
        )

import Api.Account
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Account as Account exposing (Account(..), FieldInput(..))
import Manager.Currency as Currency exposing (Currency)
import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Messages exposing (Msg(..))


view : Material.Model -> Currency -> Dict Int Account -> Html Msg
view mdl currency accounts =
    let
        -- 4 fields + 2 buttons: delete, save
        accountFields =
            4 + 2
    in
    div
        []
        ((accounts
            |> Dict.filter (\aid _ -> aid > Account.defaultId)
            |> Dict.values
            |> List.indexedMap (\i rr -> account mdl currency (i * accountFields) rr)
         )
            ++ (Dict.get 0 accounts
                    |> Maybe.map
                        (\account ->
                            [ addAccount
                                mdl
                                currency
                                (Dict.size accounts * accountFields)
                                account
                            ]
                        )
                    |> Maybe.withDefault []
               )
        )


account : Material.Model -> Currency -> Int -> Account -> Html Msg
account mdl currency baseIndex account =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ nameField
                mdl
                (baseIndex + 0)
                account
                (Request [ Api.Account.save (Account.id account) ])
            ]
        , Options.div
            []
            [ balanceField
                mdl
                currency
                (baseIndex + 1)
                account
                (Request [ Api.Account.save (Account.id account) ])
            ]
        , Options.div
            []
            [ bankAccountField
                mdl
                (baseIndex + 2)
                account
                (Request [ Api.Account.save (Account.id account) ])
            ]
        , Options.div
            []
            [ bankNameField
                mdl
                (baseIndex + 3)
                account
                (Request [ Api.Account.save (Account.id account) ])
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
                    mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple
                    , Options.onClick <| Request [ Api.Account.remove (Account.id account) ]
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
                    mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple
                    , Options.onClick <| Request [ Api.Account.save (Account.id account) ]
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


nameField : Material.Model -> Int -> Account -> Msg -> Html Msg
nameField mdl index account action =
    Textfield.render Mdl
        [ index ]
        mdl
        [ Textfield.label "Name"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Textfield.error "An non-empty account name is required"
            |> Options.when (String.length (Account.name account) <= 0)
        , Account.name account
            |> Textfield.value
        , TextInput (Account.fieldInput Validate (Account.id account) Account.Name)
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


balanceField : Material.Model -> Currency -> Int -> Account -> Msg -> Html Msg
balanceField mdl currency index account action =
    Textfield.render Mdl
        [ index ]
        mdl
        [ Textfield.label "Initial balance"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Textfield.error "A numeric value with at max. 2 decimal places"
            |> Options.when
                (let
                    (Account drec) =
                        account
                 in
                 Currency.validateAmount currency (Account.initialBalance currency account)
                    |> Maybe.map (\_ -> False)
                    |> Maybe.withDefault True
                )
        , Account.initialBalance currency account
            |> Textfield.value
        , TextInput (Account.fieldInput Collect (Account.id account) Account.InitialBalance)
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


bankAccountField : Material.Model -> Int -> Account -> Msg -> Html Msg
bankAccountField mdl index account action =
    Textfield.render Mdl
        [ index ]
        mdl
        [ Textfield.label "Bank account number"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Account.bankAccount account
            |> Textfield.value
        , TextInput (Account.fieldInput Validate (Account.id account) Account.BankAccount)
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


bankNameField : Material.Model -> Int -> Account -> Msg -> Html Msg
bankNameField mdl index account action =
    Textfield.render Mdl
        [ index ]
        mdl
        [ Textfield.label "Bank name"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Account.bankName account
            |> Textfield.value
        , TextInput (Account.fieldInput Validate (Account.id account) Account.BankName)
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


addAccount : Material.Model -> Currency -> Int -> Account -> Html Msg
addAccount mdl currency index account =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ nameField
                mdl
                (index + 1)
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            []
            [ balanceField
                mdl
                currency
                (index + 2)
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            []
            [ bankAccountField
                mdl
                (index + 3)
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            []
            [ bankNameField
                mdl
                (index + 4)
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ index + 5 ]
                mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <| Request [ Api.Account.add ]
                ]
                [ text "Add" ]
            ]
        ]
