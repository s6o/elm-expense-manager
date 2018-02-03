module Route
    exposing
        ( Route(..)
        , State(..)
        , Tab
        , defaultRoute
        , initTabs
        , tabs
        , toFragment
        , toRoute
        )

import Dict exposing (Dict)
import Navigation exposing (Location)
import UrlParser exposing ((</>), int, s, string)


type Route
    = Empty
    | Login
    | Currency
    | Accounts
    | Categories State
    | Transactions
    | Statistics
    | Groups
    | Logout


type State
    = All
    | EditId Int
    | EditKey String
    | New


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
    , Tab 3 (Categories All) "Categories"
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
            "#/login"

        Currency ->
            "#/currency"

        Accounts ->
            "#/accounts"

        Categories state ->
            "#/categories"
                ++ (case state of
                        EditId id ->
                            "/edit/" ++ toString id

                        _ ->
                            ""
                   )

        Transactions ->
            "#/transactions"

        Statistics ->
            "#/statistics"

        Groups ->
            "#/groups"

        Logout ->
            "#/logout"


toRoute : Location -> Route
toRoute location =
    let
        route =
            UrlParser.oneOf
                [ UrlParser.map Login (s "login")
                , UrlParser.map Currency (s "currency")
                , UrlParser.map Accounts (s "accounts")
                , UrlParser.map (Categories All) (s "categories")
                , UrlParser.map (EditId >> Categories) (s "categories" </> s "edit" </> int)
                , UrlParser.map Transactions (s "transactions")
                , UrlParser.map Statistics (s "statistics")
                , UrlParser.map Groups (s "groups")
                , UrlParser.map Logout (s "logout")
                ]
    in
    UrlParser.parseHash route location
        |> Maybe.map identity
        |> Maybe.withDefault Empty
