module Api.Account
    exposing
        ( add
        , read
        , remove
        , save
        )

import Api.Http as AHttp exposing (ApiPath(..))
import DRec exposing (DError, DRec)
import Dict
import Json.Decode
import Manager.Account as Account exposing (Account(..))
import Manager.Jwt as Jwt
import Meld exposing (Error(..), Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


apiPath : ApiPath
apiPath =
    ApiPath "/accounts"


add : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
add meld =
    let
        drecFn f m =
            Dict.get Account.defaultId (f m)
                |> Maybe.map (\(Account drec) -> drec)
    in
    Account.validate Account.defaultId meld
        |> Task.andThen (AHttp.post apiPath (drecFn .accounts))
        |> Task.map
            (\drec ->
                let
                    taskModel ma =
                        { ma
                            | messages = Just "Saved."
                            , accounts =
                                Dict.insert (Account.id (Account drec)) (Account drec) ma.accounts
                                    |> Dict.insert Account.defaultId (Account.empty <| Jwt.userId ma.claims)
                        }
                in
                Meld.withMerge taskModel meld
            )


read : Meld Model Error Msg -> Task Error (Meld Model Error Msg)
read meld =
    let
        model =
            Meld.model meld

        (Account drec) =
            Account.init

        decoder =
            DRec.decoder drec |> Json.Decode.map Account

        params =
            "mgr_id=eq." ++ (Basics.toString <| Jwt.userId model.claims)
    in
    AHttp.get (AHttp.apiPathParams params apiPath) decoder meld
        |> Task.map
            (\results ->
                let
                    taskModel ma =
                        { ma
                            | accounts =
                                results
                                    |> List.map (\a -> ( Account.id a, a ))
                                    |> (\accounts ->
                                            ( Account.defaultId
                                            , Account.empty <| Jwt.userId ma.claims
                                            )
                                                :: accounts
                                       )
                                    |> Dict.fromList
                        }
                in
                Meld.withMerge taskModel meld
            )


remove : Int -> Meld Model Error Msg -> Task Error (Meld Model Error Msg)
remove accountId meld =
    let
        model =
            Meld.model meld

        params =
            "pk_id=eq." ++ (accountId |> Basics.toString)
    in
    Dict.get accountId model.accounts
        |> Maybe.map
            (\_ ->
                AHttp.delete (AHttp.apiPathParams params apiPath) meld
                    |> Task.map
                        (\dmeld ->
                            let
                                taskModel ma =
                                    { ma
                                        | messages = Just "Moved to Trash."
                                        , accounts = Dict.remove accountId ma.accounts
                                    }
                            in
                            Meld.withMerge taskModel dmeld
                        )
            )
        |> Maybe.withDefault
            ("Uknown account id: "
                ++ toString accountId
                |> EMsg
                |> Task.fail
            )


save : Int -> Meld Model Error Msg -> Task Error (Meld Model Error Msg)
save accountId meld =
    let
        params =
            "pk_id=eq." ++ (accountId |> Basics.toString)

        drecFn f m =
            Dict.get accountId (f m)
                |> Maybe.map (\(Account drec) -> drec)
    in
    Account.validate accountId meld
        |> Task.andThen (AHttp.patch (AHttp.apiPathParams params apiPath) (drecFn .accounts))
        |> Task.map
            (\pmeld ->
                let
                    taskModel ma =
                        { ma | messages = Just "Saved." }
                in
                Meld.withMerge taskModel pmeld
            )
