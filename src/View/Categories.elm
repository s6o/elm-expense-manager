module View.Categories
    exposing
        ( view
        )

import Api.Category
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import KeyEvent
import Manager.Category as Category exposing (Category(..), CategoryManagement)
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
                [ Dict.get Category.defaultId mgmt.items
                    |> Maybe.map (add mdl)
                    |> Maybe.withDefault (text "Add category setup failure")
                , Options.div
                    [ Elevation.e4
                    , css "padding" "5px"
                    ]
                    [ Lists.ul
                        [ css "margin" "0"
                        , css "padding" "0"
                        ]
                        (mgmt.items
                            |> Dict.filter Category.filterMain
                            |> Dict.values
                            |> List.sortWith Category.sortWithName
                            |> (\mains ->
                                    List.indexedMap
                                        (\i c -> item mdl (i + 2) c mgmt.marked)
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

                            --                , Options.onClick <| Request [ Api.Account.add ]
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
        )


add : Material.Model -> Category -> Html Msg
add mdl category =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ Textfield.render Mdl
                [ 0 ]
                mdl
                [ Textfield.label "Category name"
                , Textfield.floatingLabel
                , css "width" "100%"
                , Textfield.error "An non-empty category name is required"
                    |> Options.when (String.length (Category.name category) <= 0)
                , Category.name category
                    |> Textfield.value
                , TextInput (Category.nameInput (Category.id category))
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
                [ 1 ]
                mdl
                [ Button.colored
                , Button.raised
                , Button.ripple

                --                , Options.onClick <| Request [ Api.Account.add ]
                ]
                [ text "Add" ]
            ]
        ]


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
