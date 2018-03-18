module View.Categories
    exposing
        ( view
        )

import Api.Category
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Category as Category exposing (Category(..), CategoryManagement, NameAction(..))
import Material
import Material.Button as Button
import Material.Color as Color
import Material.Elevation as Elevation
import Material.List as Lists
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typography
import Messages exposing (Msg(..))
import Set exposing (Set)


view : Material.Model -> Maybe CategoryManagement -> Html Msg
view mdl mm =
    div
        []
        (case mm of
            Nothing ->
                []

            Just mgmt ->
                parent mdl mgmt ++ new mdl mgmt ++ categories mdl mgmt
        )


{-| Create parent selection if a main category is selected.
-}
parent : Material.Model -> CategoryManagement -> List (Html Msg)
parent mdl mgmt =
    case mgmt.subselected of
        Nothing ->
            mgmt.selected
                |> Maybe.map (\c -> [ edit mdl c ])
                |> Maybe.withDefault []

        Just sc ->
            [ edit mdl sc ]


{-| Create add section.
-}
new : Material.Model -> CategoryManagement -> List (Html Msg)
new mdl mgmt =
    let
        baseIndex =
            mgmt.selected
                |> Maybe.map (\_ -> 3)
                |> Maybe.withDefault 0
    in
    mgmt.new
        |> Maybe.map
            (\c ->
                case mgmt.subselected of
                    Nothing ->
                        [ add mdl baseIndex c ]

                    Just _ ->
                        []
            )
        |> Maybe.withDefault []


{-| Create list of categories.
-}
categories : Material.Model -> CategoryManagement -> List (Html Msg)
categories mdl mgmt =
    let
        catFilter =
            mgmt.selected
                |> Maybe.map
                    (\c ->
                        Dict.get (Category.id c) mgmt.items
                            |> Maybe.map (Category.name >> Category.filterSub)
                            |> Maybe.withDefault Category.filterMain
                    )
                |> Maybe.withDefault Category.filterMain

        filtered =
            mgmt.items
                |> Dict.filter catFilter

        baseIndex =
            mgmt.selected
                |> Maybe.map (\_ -> 5)
                |> Maybe.withDefault 2

        subSelected =
            case mgmt.subselected of
                Nothing ->
                    False

                Just _ ->
                    True
    in
    if Dict.size filtered > 0 && not subSelected then
        [ Options.div
            [ Elevation.e4
            , css "padding" "5px"
            ]
            [ Lists.ul
                [ css "margin" "0"
                , css "padding" "0"
                ]
                (filtered
                    |> Dict.values
                    |> List.sortWith Category.sortWithName
                    |> (\mains ->
                            List.indexedMap
                                (\i c -> item mdl (i + baseIndex) c mgmt.marked)
                                mains
                       )
                )
            , Options.div
                [ css "text-align" "right"
                , css "padding" "10px 0 10px 0"
                ]
                [ Button.render Mdl
                    [ 1 ]
                    mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple

                    --                , Options.onClick <| Act [ Api.Account.add ]
                    ]
                    [ text "Remove" ]
                ]
            ]
        ]
    else
        []


{-| Add form.
-}
add : Material.Model -> Int -> Category -> Html Msg
add mdl index category =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ index ]
                mdl
                [ Textfield.label "Category name"
                , Textfield.floatingLabel
                , css "width" "100%"
                , Textfield.error "An non-empty category name is required"
                    |> Options.when (String.length (Category.name category) <= 0)
                , Category.name category
                    |> Textfield.value
                , TextInput (Category.nameInput Category.New)
                    |> Options.onInput

                --                , KeyEvent.onEnter action
                ]
                []
            ]
        , Options.div
            [ css "text-align" "right"
            , css "padding-bottom" "10px"
            ]
            [ Button.render Mdl
                [ index + 1 ]
                mdl
                [ Button.colored
                , Button.raised
                , Button.ripple

                --                , Options.onClick <| Act [ Api.Account.add ]
                ]
                [ text "Add" ]
            ]
        ]


{-| Edit form, for changeing existing category name.
-}
edit : Material.Model -> Category -> Html Msg
edit mdl category =
    let
        ( logo, label ) =
            if Category.parentPath category == "/" then
                ( "/", "Main Category" )
            else
                ( "//", "Sub Category - " ++ Category.parentPath category )
    in
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Lists.ul
            [ css "margin" "0"
            , css "padding" "0"
            ]
            [ Lists.li []
                [ Options.div
                    [ Options.center
                    , Color.background Color.accent
                    , Color.text Color.accentContrast
                    , Typography.title
                    , css "width" "36px"
                    , css "height" "36px"
                    , css "margin-right" "2rem"
                    ]
                    [ text logo
                    ]
                , Lists.content
                    []
                    [ Options.span
                        []
                        [ Textfield.render Mdl
                            [ 0 ]
                            mdl
                            [ Textfield.label label
                            , Textfield.floatingLabel
                            , css "width" "100%"
                            , Textfield.error "An non-empty category name is required"
                                |> Options.when (String.length (Category.name category) <= 0)
                            , Category.name category
                                |> Textfield.value
                            , TextInput (Category.nameInput Category.Edit)
                                |> Options.onInput

                            --                , KeyEvent.onEnter action
                            ]
                            []
                        ]
                    ]
                ]
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
                    [ 2 ]
                    mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple
                    , Options.onClick <| Act [ Category.unselect ]
                    ]
                    [ text "Up" ]
                ]
            , Options.div
                [ css "text-align" "right"
                , css "padding-bottom" "10px"
                , css "float" "right"
                ]
                [ Button.render Mdl
                    [ 1 ]
                    mdl
                    [ Button.colored
                    , Button.raised
                    , Button.ripple

                    --                , Options.onClick <| Act [ Api.Account.add ]
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


{-| Material List entry for a category.
-}
item : Material.Model -> Int -> Category -> Set Int -> Html Msg
item mdl index c marked =
    let
        checkMark =
            Set.member (Category.id c) marked
    in
    Lists.li [ Lists.withSubtitle ]
        [ Options.div
            [ Options.center
            , Color.background Color.accent
            , Color.text Color.accentContrast
            , Typography.title
            , css "width" "36px"
            , css "height" "36px"
            , css "margin-right" "2rem"
            ]
            [ Category.name c
                |> String.left 1
                |> text
            ]
        , Lists.content
            [ css "cursor" "pointer"
            , Options.onClick <| Act [ Category.select <| Category.id c ]
            ]
            [ Options.span
                []
                [ Category.name c
                    |> text
                ]
            , Lists.subtitle
                []
                [ Category.parentPath c
                    |> text
                ]
            ]
        , Lists.content2 []
            [ Toggles.checkbox Mdl
                [ index ]
                mdl
                [ Toggles.value checkMark
                , Act [ Category.toggle (Category.id c) (not checkMark) ]
                    |> Options.onToggle
                ]
                []
            ]
        ]
