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
    | Currency
    | Accounts
    | Categories
    | Transactions
    | Statistics
    | Groups
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
    , Tab 1 Currency "Currency"
    , Tab 2 Accounts "Accounts"
    , Tab 3 Categories "Categories"
    , Tab 4 Transactions "Transactions"
    , Tab 5 Statistics "Statistics"
    , Tab 6 Groups "Groups"
    , Tab 7 Logout "Logout"
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

        Currency ->
            "#currency"

        Accounts ->
            "#accounts"

        Categories ->
            "#categories"

        Transactions ->
            "#transactions"

        Statistics ->
            "#statistics"

        Groups ->
            "#groups"

        Logout ->
            "#logout"
