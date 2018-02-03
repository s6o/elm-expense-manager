module View.Logout
    exposing
        ( view
        )

import Api.Auth
import Html exposing (Html, div, text)
import Material
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css)
import Messages exposing (Msg(..))
import Navigation exposing (Location)
import Route


view : Material.Model -> Maybe String -> Maybe Location -> Html Msg
view mdl token mlocation =
    Options.div
        [ Elevation.e4
        , css "padding" "5px"
        ]
        [ Options.div
            []
            [ text "Would you like to logout?"
            ]
        , Options.div
            [ css "text-align" "right"
            ]
            [ Button.render Mdl
                [ 0 ]
                mdl
                [ Button.colored
                , Button.raised
                , Button.ripple
                , Options.onClick <|
                    SelectTab
                        (mlocation
                            |> Maybe.map
                                (\l -> { l | hash = Route.defaultRoute token |> Route.toFragment })
                        )
                , css "margin-right" "10px"
                ]
                [ text "No" ]
            , Button.render Mdl
                [ 1 ]
                mdl
                [ Button.raised
                , Button.ripple
                , Options.onClick <| Act [ Api.Auth.logout ]
                ]
                [ text "Yes" ]
            ]
        ]
