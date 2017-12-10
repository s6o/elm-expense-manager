module Route
    exposing
        ( Route(..)
        , Tab
        , defaultRoute
        , initTabs
        , tabs
        , toFragment
        )

import Dict exposing (Dict)


type Route
    = Empty
    | Login
    | Accounts
    | Categories
    | Transactions
    | Statistics
    | Settings
    | Logout


type alias Tab =
    { index : Int
    , route : Route
    , label : String
    }


defaultRoute : Maybe String -> Route
defaultRoute token =
    case token of
        Nothing ->
            Login

        Just _ ->
            Transactions


initTabs : Dict String Tab
initTabs =
    [ Tab 0 Login "Login"
    , Tab 1 Accounts "Accounts"
    , Tab 2 Categories "Categories"
    , Tab 3 Transactions "Transactions"
    , Tab 4 Statistics "Statistics"
    , Tab 5 Settings "Settings"
    , Tab 6 Logout "Logout"
    ]
        |> List.map (\t -> ( toFragment t.route, t ))
        |> Dict.fromList


tabs : Maybe String -> Dict String Tab -> List Tab
tabs token tabs =
    case token of
        Nothing ->
            Dict.get (defaultRoute token |> toFragment) tabs
                |> Maybe.map (\t -> [ t ])
                |> Maybe.withDefault []

        Just _ ->
            Dict.values tabs
                |> List.sortBy .index
                |> List.drop 1


toFragment : Route -> String
toFragment route =
    case route of
        Empty ->
            ""

        Login ->
            "#login"

        Accounts ->
            "#accounts"

        Categories ->
            "#categories"

        Transactions ->
            "#transactions"

        Statistics ->
            "#statistics"

        Settings ->
            "#settings"

        Logout ->
            "#logout"
