module DRec
    exposing
        ( DError(..)
        , DField
        , DRec
        , DType(..)
        , DValue(..)
        , clear
        , decoder
        , empty
        , errorMessage
        , field
        , fromBool
        , fromFloat
        , fromInt
        , fromJson
        , fromMaybe
        , fromObject
        , fromString
        , fromStringObject
        , get
        , hasSchema
        , isEmpty
        , setBool
        , setFloat
        , setInt
        , setJson
        , setString
        , setWith
        , toBool
        , toFloat
        , toInt
        , toJson
        , toMaybe
        , toObject
        , toString
        )

{-| Elm `Dict` based record with field name and type validation and automatic
decoding from and to JSON.


# Build


## Schema

@docs DType, DValue, DRec, DField, DError, empty, errorMessage, field


## Values

@docs clear, setBool, setFloat, setInt, setJson, setString, setWith


## JSON interop

@docs decoder, fromObject, fromStringObject, toObject


# Query

@docs get, hasSchema, isEmpty


# Decode

@docs fromBool, fromFloat, fromInt, fromJson, fromMaybe, fromString

Decode from Elm types.


# Encode

@docs toBool, toFloat, toInt, toJson, toMaybe, toString

Encode to Elm types.

-}

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Encode


-- BUILD


{-| `DRec` schema types.
-}
type DType
    = DNever
    | DBool
    | DFloat
    | DInt
    | DJson
    | DMaybe DValue
    | DString


{-| Sub-type for scalar values.
-}
type DValue
    = VNil
    | VBool
    | VFloat
    | VInt
    | VJson
    | VString


{-| A record with schema.
-}
type DRec
    = DRec
        { fields : List String
        , schema : Dict String DType
        , store : Dict String DField
        }


{-| `DRec` field name and `DType` mapping, see `field`.
-}
type DField
    = DBool_ Bool
    | DFloat_ Float
    | DInt_ Int
    | DJson_ Json.Encode.Value
    | DMaybe_ (Maybe DField)
    | DString_ String


{-| @private
-}
fieldType : DField -> DType
fieldType dfield =
    case dfield of
        DBool_ _ ->
            DBool

        DFloat_ _ ->
            DFloat

        DInt_ _ ->
            DInt

        DJson_ _ ->
            DJson

        DMaybe_ mf ->
            case mf of
                Nothing ->
                    DMaybe VNil

                Just f ->
                    case fieldType f of
                        DBool ->
                            DMaybe VBool

                        DFloat ->
                            DMaybe VFloat

                        DInt ->
                            DMaybe VInt

                        DJson ->
                            DMaybe VJson

                        DString ->
                            DMaybe VString

                        _ ->
                            DNever

        DString_ _ ->
            DString


{-| Error tags
-}
type DError
    = DecodingFailed String
    | DuplicateField String
    | InvalidSchemaType String
    | MissingValue String
    | NoSchema
    | TypeMismatch String
    | UknownField String


{-| Create an empty `DRec`.
-}
empty : Result DError DRec
empty =
    DRec
        { fields = []
        , schema = Dict.empty
        , store = Dict.empty
        }
        |> Ok


{-| Get error message.
-}
errorMessage : Result DError DRec -> Maybe String
errorMessage rr =
    case rr of
        Ok _ ->
            Nothing

        Err de ->
            case de of
                DecodingFailed msg ->
                    Just ("Decoding failed | " ++ msg)

                DuplicateField msg ->
                    Just ("Duplicate field | " ++ msg)

                InvalidSchemaType msg ->
                    Just ("Invalid schema type | " ++ msg)

                MissingValue msg ->
                    Just ("Missing value | " ++ msg)

                NoSchema ->
                    Just "No schema"

                TypeMismatch msg ->
                    Just ("Type mismatch | " ++ msg)

                UknownField msg ->
                    Just ("Unknown field | " ++ msg)


{-| Define `DRec` schema when initializing your application's model member.

    type alias Model =
        { user : Result DError DRec
        }

    init : Model
    init =
        { user =
            DRec.empty
                |> DRec.field "id" DInt
                |> DRec.field "email" DString
                |> DRec.field "name" DString
                |> DRec.field "token" DMaybe VString
        }

-}
field : String -> DType -> Result DError DRec -> Result DError DRec
field field dtype rr =
    let
        typeError dt =
            Basics.toString dtype
                |> InvalidSchemaType
                |> Err
    in
    case dtype of
        DNever ->
            typeError dtype

        DMaybe VNil ->
            typeError dtype

        _ ->
            case rr of
                Err x ->
                    Err x

                Ok (DRec r) ->
                    case
                        Dict.get field r.schema
                    of
                        Nothing ->
                            DRec
                                { r
                                    | fields = r.fields ++ [ field ]
                                    , schema = Dict.insert field dtype r.schema
                                }
                                |> Ok

                        Just _ ->
                            field
                                |> DuplicateField
                                |> Err


{-| Remove all data from `DRec`, schema is not affected.
-}
clear : Result DError DRec -> Result DError DRec
clear rr =
    case rr of
        Err x ->
            Err x

        Ok (DRec r) ->
            DRec { r | store = Dict.empty }
                |> Ok


{-| Set a `Bool` value for specified `DRec` field.
-}
setBool : String -> Bool -> Result DError DRec -> Result DError DRec
setBool field value rr =
    setWith field fromBool value rr


{-| Set a `Float` value for specified `DRec` field.
-}
setFloat : String -> Float -> Result DError DRec -> Result DError DRec
setFloat field value rr =
    setWith field fromFloat value rr


{-| Set a `Int` value for specified `DRec` field.
-}
setInt : String -> Int -> Result DError DRec -> Result DError DRec
setInt field value rr =
    setWith field fromInt value rr


{-| Set a `Json.Encode.Value` value for specified `DRec` field.
-}
setJson : String -> Json.Encode.Value -> Result DError DRec -> Result DError DRec
setJson field value rr =
    setWith field fromJson value rr


{-| Set a `String` value for specified `DRec` field.
-}
setString : String -> String -> Result DError DRec -> Result DError DRec
setString field value rr =
    setWith field fromString value rr


{-| Set a value for specified `DRec` field with a custom value conversion/validation.

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        Email str ->
            ( { model | user = DRec.setString "email" str model.user }
            , Cmd.none
            )

        Token mstr ->
            ( { model | user = DRec.setWith "token" (DRec.fromMaybe DRec.fromString) mstr model.user}
            , Cmd.none
            )

-}
setWith : String -> (a -> DField) -> a -> Result DError DRec -> Result DError DRec
setWith field toValue value rr =
    case rr of
        Err x ->
            Err x

        Ok (DRec r) ->
            case
                Dict.get field r.schema
            of
                Nothing ->
                    field
                        |> UknownField
                        |> Err

                Just dt ->
                    if fieldType (toValue value) == dt then
                        DRec { r | store = Dict.insert field (toValue value) r.store }
                            |> Ok
                    else
                        (Basics.toString (toValue value) ++ " /= " ++ Basics.toString dt)
                            |> TypeMismatch
                            |> Err



-- QUERY


{-| For a valid field defined in schema return a value/type mapping.
-}
get : String -> Result DError DRec -> Result DError DField
get field rr =
    case rr of
        Err x ->
            Err x

        Ok (DRec r) ->
            case
                Dict.get field r.schema
            of
                Nothing ->
                    field
                        |> UknownField
                        |> Err

                Just _ ->
                    case
                        Dict.get field r.store
                    of
                        Nothing ->
                            field
                                |> MissingValue
                                |> Err

                        Just dfield ->
                            Ok dfield


{-| Check if a schema has been specified.
-}
hasSchema : Result DError DRec -> Bool
hasSchema rr =
    case rr of
        Err _ ->
            False

        Ok (DRec r) ->
            not <| Dict.isEmpty r.schema


{-| Check is specified `DRec` contains data.
-}
isEmpty : Result DError DRec -> Bool
isEmpty rr =
    case rr of
        Err _ ->
            True

        Ok (DRec r) ->
            Dict.isEmpty r.store



-- ENCODE


{-| -}
fromBool : Bool -> DField
fromBool v =
    DBool_ v


{-| -}
fromFloat : Float -> DField
fromFloat v =
    DFloat_ v


{-| -}
fromInt : Int -> DField
fromInt v =
    DInt_ v


{-| -}
fromJson : Json.Encode.Value -> DField
fromJson v =
    DJson_ v


{-| -}
fromMaybe : (a -> DField) -> Maybe a -> DField
fromMaybe f mv =
    case mv of
        Nothing ->
            DMaybe_ Nothing

        Just v ->
            DMaybe_ (Just (f v))


{-| -}
fromString : String -> DField
fromString v =
    DString_ v



-- DECODE


{-| -}
toBool : Result DError DField -> Result DError Bool
toBool rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DBool_ v ->
                    Ok v

                _ ->
                    "toBool"
                        |> TypeMismatch
                        |> Err


{-| -}
toFloat : Result DError DField -> Result DError Float
toFloat rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DFloat_ v ->
                    Ok v

                _ ->
                    "toFloat"
                        |> TypeMismatch
                        |> Err


{-| -}
toInt : Result DError DField -> Result DError Int
toInt rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DInt_ v ->
                    Ok v

                _ ->
                    "toInt"
                        |> TypeMismatch
                        |> Err


{-| -}
toJson : Result DError DField -> Result DError Json.Encode.Value
toJson rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DJson_ v ->
                    Ok v

                _ ->
                    "toJson"
                        |> TypeMismatch
                        |> Err


{-| Convert from a `DField` of `DType` 'DMaybe a' to `Maybe a`.

    rec : DRec
    rec =
        DRec.empty
            |> DRec.field "token" DMaybe VString

    update : Maybe String -> DRec -> DRec
    update mv drec =
        DRec.set "token" (DRec.fromMaybe DRec.fromString) mv drec

    token : DRec -> Maybe String
    token drec =
        DRec.get "token" drec
            |> DRec.toMaybe (DRec.toString >> Result.toMaybe)

-}
toMaybe : (Result DError DField -> Maybe a) -> Result DError DField -> Result DError (Maybe a)
toMaybe toValue rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DMaybe_ m ->
                    case m of
                        Nothing ->
                            Nothing
                                |> Ok

                        Just df ->
                            toValue (Ok df)
                                |> Ok

                _ ->
                    "toMaybe"
                        |> TypeMismatch
                        |> Err


{-| -}
toString : Result DError DField -> Result DError String
toString rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DString_ v ->
                    Ok v

                _ ->
                    "toString"
                        |> TypeMismatch
                        |> Err



-- JSON interop


{-| @private
-}
fieldDecoder : String -> DType -> DRec -> Decoder DRec
fieldDecoder fname dtype drec =
    case dtype of
        DNever ->
            Json.Decode.fail "DNever is never decoded."

        DMaybe VNil ->
            Json.Decode.fail "DMaybe VNil is never decoded."

        DBool ->
            Json.Decode.field fname Json.Decode.bool
                |> Json.Decode.map
                    (\v ->
                        setWith fname fromBool v (Ok drec)
                            |> Result.withDefault drec
                    )

        DFloat ->
            Json.Decode.field fname Json.Decode.float
                |> Json.Decode.map
                    (\v ->
                        setWith fname fromFloat v (Ok drec)
                            |> Result.withDefault drec
                    )

        DInt ->
            Json.Decode.field fname Json.Decode.int
                |> Json.Decode.map
                    (\v ->
                        setWith fname fromInt v (Ok drec)
                            |> Result.withDefault drec
                    )

        DJson ->
            Json.Decode.field fname Json.Decode.value
                |> Json.Decode.map
                    (\v ->
                        setWith fname fromJson v (Ok drec)
                            |> Result.withDefault drec
                    )

        DMaybe VBool ->
            Json.Decode.maybe (Json.Decode.field fname Json.Decode.bool)
                |> Json.Decode.map
                    (\vm ->
                        setWith fname (fromMaybe fromBool) vm (Ok drec)
                            |> Result.withDefault drec
                    )

        DMaybe VFloat ->
            Json.Decode.maybe (Json.Decode.field fname Json.Decode.float)
                |> Json.Decode.map
                    (\vm ->
                        setWith fname (fromMaybe fromFloat) vm (Ok drec)
                            |> Result.withDefault drec
                    )

        DMaybe VInt ->
            Json.Decode.maybe (Json.Decode.field fname Json.Decode.int)
                |> Json.Decode.map
                    (\vm ->
                        setWith fname (fromMaybe fromInt) vm (Ok drec)
                            |> Result.withDefault drec
                    )

        DMaybe VJson ->
            Json.Decode.maybe (Json.Decode.field fname Json.Decode.value)
                |> Json.Decode.map
                    (\vm ->
                        setWith fname (fromMaybe fromJson) vm (Ok drec)
                            |> Result.withDefault drec
                    )

        DMaybe VString ->
            Json.Decode.maybe (Json.Decode.field fname Json.Decode.string)
                |> Json.Decode.map
                    (\vm ->
                        setWith fname (fromMaybe fromString) vm (Ok drec)
                            |> Result.withDefault drec
                    )

        DString ->
            Json.Decode.field fname Json.Decode.string
                |> Json.Decode.map
                    (\v ->
                        setWith fname fromString v (Ok drec)
                            |> Result.withDefault drec
                    )


{-| Create decoder for specified `DRec`.
-}
decoder : Result DError DRec -> Decoder DRec
decoder rr =
    case rr of
        Err _ ->
            errorMessage rr
                |> Maybe.map Json.Decode.fail
                |> Maybe.withDefault
                    ("DRec decoder failure. Looks like you managed to break elm-drec logic, please make a report."
                        |> Json.Decode.fail
                    )

        Ok (DRec r) ->
            r.fields
                |> List.foldl
                    (\fname accum ->
                        Dict.get fname r.schema
                            |> Maybe.map (\dtype -> Json.Decode.andThen (fieldDecoder fname dtype) accum)
                            |> Maybe.withDefault accum
                    )
                    (DRec r |> Json.Decode.succeed)


{-| Initialize `DRec` data by decoding specified JSON (`Json.Encode.Value`) accordingly to `DRec` schema.
-}
fromObject : Result DError DRec -> Json.Encode.Value -> Result DError DRec
fromObject rr json =
    case rr of
        Err x ->
            Err x

        Ok drec ->
            if hasSchema rr then
                Json.Decode.decodeValue (decoder rr) json
                    |> Result.mapError DecodingFailed
            else
                Err NoSchema


{-| Initialize `DRec` data by decoding specified JSON string literal accordingly to `DRec` schema.
-}
fromStringObject : Result DError DRec -> String -> Result DError DRec
fromStringObject rr json =
    case rr of
        Err x ->
            Err x

        Ok drec ->
            if hasSchema rr then
                Json.Decode.decodeString (decoder rr) json
                    |> Result.mapError DecodingFailed
            else
                Err NoSchema


{-| Encode `DRec` into a JSON object accordingly to `DRec` schema.
-}
toObject : Result DError DRec -> Json.Encode.Value
toObject rr =
    case rr of
        Err x ->
            Json.Encode.object []

        Ok (DRec r) ->
            r.fields
                |> List.foldr
                    (\field accum ->
                        Dict.get field r.store
                            |> Maybe.map
                                (\dfield ->
                                    case dfield of
                                        DBool_ b ->
                                            ( field, Json.Encode.bool b ) :: accum

                                        DFloat_ f ->
                                            ( field, Json.Encode.float f ) :: accum

                                        DInt_ i ->
                                            ( field, Json.Encode.int i ) :: accum

                                        DJson_ v ->
                                            ( field, v ) :: accum

                                        DMaybe_ mv ->
                                            case mv of
                                                Nothing ->
                                                    accum

                                                Just df ->
                                                    case fieldType df of
                                                        DMaybe VNil ->
                                                            ( field, Json.Encode.null ) :: accum

                                                        DMaybe VBool ->
                                                            toBool (Ok df)
                                                                |> Result.map (\v -> ( field, Json.Encode.bool v ) :: accum)
                                                                |> Result.withDefault accum

                                                        DMaybe VFloat ->
                                                            toFloat (Ok df)
                                                                |> Result.map (\v -> ( field, Json.Encode.float v ) :: accum)
                                                                |> Result.withDefault accum

                                                        DMaybe VInt ->
                                                            toInt (Ok df)
                                                                |> Result.map (\v -> ( field, Json.Encode.int v ) :: accum)
                                                                |> Result.withDefault accum

                                                        DMaybe VJson ->
                                                            toJson (Ok df)
                                                                |> Result.map (\v -> ( field, v ) :: accum)
                                                                |> Result.withDefault accum

                                                        DMaybe VString ->
                                                            toString (Ok df)
                                                                |> Result.map (\v -> ( field, Json.Encode.string v ) :: accum)
                                                                |> Result.withDefault accum

                                                        _ ->
                                                            accum

                                        DString_ s ->
                                            ( field, Json.Encode.string s ) :: accum
                                )
                            |> Maybe.withDefault accum
                    )
                    []
                |> Json.Encode.object
