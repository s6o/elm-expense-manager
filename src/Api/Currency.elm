module Api.Currency
    exposing
        ( read
        , save
        )

import Api.Http as AHttp exposing (ApiPath(..))
import DRec exposing (DRec)
import Manager.Currency as Currency exposing (Currency(..))
import Meld exposing (Error, Meld)
import Messages exposing (Msg)
import Model exposing (Model)
import Task exposing (Task)


apiPath : ApiPath
apiPath =
    ApiPath "/currency"


read : Meld Model Msg -> Task (Error Model) (Meld Model Msg)
read meld =
    let
        model =
            Meld.model meld

        (Currency drec) =
            model.currency
    in
    AHttp.getSingle apiPath (DRec.decoder drec) meld
        |> Task.map
            (\crec ->
                let
                    taskModel ma =
                        { ma | currency = Currency crec }
                in
                Meld.withMerge taskModel meld
            )


save : Meld Model Msg -> Task (Error Model) (Meld Model Msg)
save meld =
    let
        model =
            Meld.model meld

        (Currency drec) =
            model.currency

        drecFn f m =
            f m
                |> (\(Currency drec) -> Just drec)
    in
    Currency.validate meld
        |> Task.andThen (AHttp.patch apiPath (drecFn .currency))
        |> Task.map
            (\pmeld ->
                let
                    taskModel ma =
                        { ma | messages = Just "Saved." }
                in
                Meld.withMerge taskModel pmeld
            )
