module View.Accounts
    exposing
        ( view
        )

import Api.Account
import Dict
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Account as Account exposing (Account(..), FieldInput(..))
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
        ((model.accounts
            |> Dict.filter (\aid _ -> aid > Account.defaultId)
            |> Dict.values
            |> List.indexedMap (\i rr -> account model (i * accountFields) rr)
         )
            ++ (Dict.get 0 model.accounts
                    |> Maybe.map
                        (\account ->
                            [ addAccount
                                (Dict.size model.accounts * accountFields)
                                model
                                account
                            ]
                        )
                    |> Maybe.withDefault []
               )
        )


account : Model -> Int -> Account -> Html Msg
account model baseIndex account =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ nameField
                (baseIndex + 0)
                model
                account
                (Request [ Api.Account.save (Account.id account) ])
            ]
        , Options.div
            []
            [ balanceField
                (baseIndex + 1)
                model
                account
                (Request [ Api.Account.save (Account.id account) ])
            ]
        , Options.div
            []
            [ bankAccountField
                (baseIndex + 2)
                model
                account
                (Request [ Api.Account.save (Account.id account) ])
            ]
        , Options.div
            []
            [ bankNameField
                (baseIndex + 3)
                model
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
                    , Options.onClick <| Request [ Api.Account.save (Account.id account) ]
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


nameField : Int -> Model -> Account -> Msg -> Html Msg
nameField index model account action =
    Textfield.render Mdl
        [ index ]
        model.mdl
        [ Textfield.label "Name"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Textfield.error "An non-empty account name is required"
            |> Options.when (String.length (Account.name account) <= 0)
        , Account.name account
            |> Textfield.value
        , TextInput (Account.fieldInput Validate (Account.id account) "name") model
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


balanceField : Int -> Model -> Account -> Msg -> Html Msg
balanceField index model account action =
    Textfield.render Mdl
        [ index ]
        model.mdl
        [ Textfield.label "Initial balance"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Textfield.error "A numeric value with at max. 2 decimal places"
            |> Options.when
                (let
                    (Account drec) =
                        account
                 in
                 Currency.validateAmount model.currency (Account.initialBalance model.currency account)
                    |> Maybe.map (\_ -> False)
                    |> Maybe.withDefault True
                )
        , Account.initialBalance model.currency account
            |> Textfield.value
        , TextInput (Account.fieldInput Collect (Account.id account) "initial_balance") model
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


bankAccountField : Int -> Model -> Account -> Msg -> Html Msg
bankAccountField index model account action =
    Textfield.render Mdl
        [ index ]
        model.mdl
        [ Textfield.label "Bank account number"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Account.bankAccount account
            |> Textfield.value
        , TextInput (Account.fieldInput Validate (Account.id account) "bank_account") model
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


bankNameField : Int -> Model -> Account -> Msg -> Html Msg
bankNameField index model account action =
    Textfield.render Mdl
        [ index ]
        model.mdl
        [ Textfield.label "Bank name"
        , Textfield.floatingLabel
        , css "width" "100%"
        , Account.bankName account
            |> Textfield.value
        , TextInput (Account.fieldInput Validate (Account.id account) "bank_name") model
            |> Options.onInput
        , KeyEvent.onEnter action
        ]
        []


addAccount : Int -> Model -> Account -> Html Msg
addAccount index model account =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ nameField
                (index + 1)
                model
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            []
            [ balanceField
                (index + 2)
                model
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            []
            [ bankAccountField
                (index + 3)
                model
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            []
            [ bankNameField
                (index + 4)
                model
                account
                (Request [ Api.Account.add ])
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ index + 5 ]
                model.mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <| Request [ Api.Account.add ]
                ]
                [ text "Add" ]
            ]
        ]
