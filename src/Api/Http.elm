module Api.Http
    exposing
        ( ApiPath(..)
        , apiPathParams
        , delete
        , get
        , getSingle
        , patch
        , post
        , postMap
        )

{-| Basic HTTP operations for PostgREST
-}

import Api.Headers exposing (objectHeader, recordHeader, tokenHeader)
import DRec exposing (DError, DRec)
import Http
import HttpBuilder exposing (..)
import Json.Decode exposing (Decoder)
import Meld exposing (Error(..), Meld)
import Task exposing (Task)


{-| `Model` requirements.
-}
type alias Parent m =
    { m
        | apiBaseUrl : String
        , token : Maybe String
    }


{-| A URL path mapping a PostgREST resource, may include parameters (e.g. query string)
-}
type ApiPath
    = ApiPath String


apiPathParams : String -> ApiPath -> ApiPath
apiPathParams params (ApiPath path) =
    path
        ++ "?"
        ++ params
        |> ApiPath


delete : ApiPath -> Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
delete (ApiPath path) meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ path
        |> HttpBuilder.delete
        |> withHeaders (tokenHeader model.token)
        |> withExpect Http.expectString
        |> HttpBuilder.toTask
        |> Task.mapError (Meld.EHttp model)
        |> Task.map (\_ -> meld)


get : ApiPath -> Decoder a -> Meld (Parent m) msg -> Task (Error (Parent m)) (List a)
get (ApiPath path) decoder meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ path
        |> HttpBuilder.get
        |> withHeaders (tokenHeader model.token)
        |> withExpect (Http.expectJson (Json.Decode.list decoder))
        |> HttpBuilder.toTask
        |> Task.mapError (Meld.EHttp model)


getSingle : ApiPath -> Decoder a -> Meld (Parent m) msg -> Task (Error (Parent m)) a
getSingle (ApiPath path) decoder meld =
    let
        model =
            Meld.model meld
    in
    model.apiBaseUrl
        ++ path
        |> HttpBuilder.get
        |> withHeaders (objectHeader ++ tokenHeader model.token)
        |> withExpect (Http.expectJson decoder)
        |> HttpBuilder.toTask
        |> Task.mapError (Meld.EHttp model)


patch : ApiPath -> (Parent m -> Maybe (DRec a)) -> Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
patch (ApiPath path) drecFn meld =
    let
        model =
            Meld.model meld
    in
    drecFn model
        |> Maybe.map
            (\drec ->
                model.apiBaseUrl
                    ++ path
                    |> HttpBuilder.patch
                    |> withHeaders (tokenHeader model.token)
                    |> withJsonBody (DRec.encoder drec)
                    |> withExpect Http.expectString
                    |> HttpBuilder.toTask
                    |> Task.mapError (Meld.EHttp model)
                    |> Task.map (\_ -> meld)
            )
        |> Maybe.withDefault
            ("Record for Api.Http.patch not found"
                |> EMsg model
                |> Task.fail
            )


post : ApiPath -> (Parent m -> Maybe (DRec a)) -> Meld (Parent m) msg -> Task (Error (Parent m)) (DRec a)
post (ApiPath path) drecFn meld =
    let
        model =
            Meld.model meld
    in
    drecFn model
        |> Maybe.map
            (\drec ->
                model.apiBaseUrl
                    ++ path
                    |> HttpBuilder.post
                    |> withHeaders (objectHeader ++ recordHeader ++ tokenHeader model.token)
                    |> withJsonBody (DRec.encoder drec)
                    |> withExpect (DRec.decoder drec |> Http.expectJson)
                    |> HttpBuilder.toTask
                    |> Task.mapError (Meld.EHttp model)
            )
        |> Maybe.withDefault
            ("Record for Api.Http.post not found"
                |> EMsg model
                |> Task.fail
            )


postMap : ApiPath -> (Parent m -> Maybe (DRec a)) -> Decoder b -> Meld (Parent m) msg -> Task (Error (Parent m)) b
postMap (ApiPath path) drecFn decoder meld =
    let
        model =
            Meld.model meld
    in
    drecFn model
        |> Maybe.map
            (\drec ->
                model.apiBaseUrl
                    ++ path
                    |> HttpBuilder.post
                    |> withHeaders (objectHeader ++ recordHeader ++ tokenHeader model.token)
                    |> withJsonBody (DRec.encoder drec)
                    |> withExpect (Http.expectJson decoder)
                    |> HttpBuilder.toTask
                    |> Task.mapError (Meld.EHttp model)
            )
        |> Maybe.withDefault
            ("Record for Api.Http.post not found"
                |> EMsg model
                |> Task.fail
            )
