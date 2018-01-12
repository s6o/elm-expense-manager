module DRec
    exposing
        ( DError(..)
        , DField
        , DRec
        , DSchema
        , DType(..)
        , DValue(..)
        , clear
        , decodeString
        , decodeValue
        , decoder
        , empty
        , encoder
        , errorMessage
        , field
        , fromArray
        , fromBool
        , fromDRec
        , fromFloat
        , fromInt
        , fromJson
        , fromList
        , fromMaybe
        , fromString
        , get
        , hasSchema
        , isEmpty
        , schema
        , setBool
        , setDRec
        , setFloat
        , setInt
        , setJson
        , setString
        , setWith
        , toArray
        , toBool
        , toDRec
        , toFloat
        , toInt
        , toJson
        , toList
        , toMaybe
        , toString
        )

{-| Elm `Dict` based record with field name and type validation and automatic
decoding from and to JSON.


# Build


## Schema

@docs DType, DValue, DRec, DSchema, DField, DError, empty, errorMessage, field


## Values

@docs clear, setBool, setDRec, setFloat, setInt, setJson, setString, setWith


## JSON interop

@docs decoder, decodeValue, decodeString, encoder


# Query

@docs get, hasSchema, isEmpty, schema


# Decode

@docs fromArray, fromBool, fromDRec, fromFloat, fromInt, fromJson, fromList, fromMaybe, fromString

Decode from Elm types.


# Encode

@docs toArray, toBool, toDRec, toFloat, toInt, toJson, toList, toMaybe, toString

Encode to Elm types.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Encode


-- BUILD


{-| `DRec` schema types.
-}
type DType
    = DNever
    | DArray DValue
    | DBool
    | DDRec DSchema
    | DFloat
    | DInt
    | DJson
    | DList DValue
    | DMaybe DValue
    | DString


{-| Sub-type for container types.
-}
type DValue
    = VNil
    | VBool
    | VDRec DSchema
    | VFloat
    | VInt
    | VJson
    | VString


{-| A record with schema.
-}
type DRec
    = DRec
        { buffers : Dict String String
        , errors : Dict String DError
        , fields : List String
        , schema : Dict String DType
        , store : Dict String DField
        }


{-| `DRec` field name and `DType` mapping, see `field`.
-}
type DField
    = DArray_ (Array DField)
    | DBool_ Bool
    | DDRec_ DRec
    | DFloat_ Float
    | DInt_ Int
    | DJson_ Json.Encode.Value
    | DList_ (List DField)
    | DMaybe_ (Maybe DField)
    | DString_ String


{-| `DRec` schema.
-}
type DSchema
    = DSchema ( List String, Dict String DType )


{-| @private
-}
fieldType : DField -> DType
fieldType dfield =
    case dfield of
        DArray_ dfa ->
            Array.get 0 dfa
                |> Maybe.map (\df -> DArray <| fieldSubType df)
                |> Maybe.withDefault (DArray VNil)

        DBool_ _ ->
            DBool

        DDRec_ (DRec r) ->
            DDRec (DSchema ( r.fields, r.schema ))

        DFloat_ _ ->
            DFloat

        DInt_ _ ->
            DInt

        DJson_ _ ->
            DJson

        DList_ dfl ->
            dfl
                |> List.head
                |> Maybe.map (\df -> DList <| fieldSubType df)
                |> Maybe.withDefault (DList VNil)

        DMaybe_ mf ->
            case mf of
                Nothing ->
                    DMaybe VNil

                Just f ->
                    DMaybe <| fieldSubType f

        DString_ _ ->
            DString


{-| @private
-}
fieldSubType : DField -> DValue
fieldSubType dfield =
    case fieldType dfield of
        DBool ->
            VBool

        DDRec dschema ->
            VDRec dschema

        DFloat ->
            VFloat

        DInt ->
            VInt

        DJson ->
            VJson

        DString ->
            VString

        _ ->
            VNil


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
empty : DRec
empty =
    DRec
        { buffers = Dict.empty
        , errors = Dict.empty
        , fields = []
        , schema = Dict.empty
        , store = Dict.empty
        }


{-| Get error message.
-}
errorMessage : DRec -> Maybe String
errorMessage (DRec r) =
    if Dict.isEmpty r.errors then
        Nothing
    else
        r.errors
            |> Dict.foldl
                (\field derror accum ->
                    accum
                        ++ ("| "
                                ++ field
                                ++ " -> "
                                ++ (case derror of
                                        DecodingFailed msg ->
                                            "Decoding failed: " ++ msg

                                        DuplicateField msg ->
                                            "Duplicate field: " ++ msg

                                        InvalidSchemaType msg ->
                                            "Invalid schema type: " ++ msg

                                        MissingValue msg ->
                                            "Missing value: " ++ msg

                                        NoSchema ->
                                            "No schema."

                                        TypeMismatch msg ->
                                            "Type mismatch: " ++ msg

                                        UknownField msg ->
                                            "Unknown field: " ++ msg
                                   )
                           )
                )
                ""
            |> Just


{-| Define `DRec` schema when initializing your application's model member.

    type alias Model =
        { user : DRec
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
field : String -> DType -> DRec -> DRec
field field dtype (DRec r) =
    let
        typeError =
            Basics.toString dtype
                |> InvalidSchemaType
                |> (\derror -> { r | errors = Dict.insert field derror r.errors })
    in
    case dtype of
        DNever ->
            DRec typeError

        DArray VNil ->
            DRec typeError

        DMaybe VNil ->
            DRec typeError

        _ ->
            case
                Dict.get field r.schema
            of
                Nothing ->
                    DRec
                        { r
                            | fields = r.fields ++ [ field ]
                            , schema = Dict.insert field dtype r.schema
                        }

                Just _ ->
                    field
                        |> DuplicateField
                        |> (\derror -> { r | errors = Dict.insert field derror r.errors })
                        |> DRec


{-| Remove all data from `DRec`, schema is not affected.
-}
clear : DRec -> DRec
clear (DRec r) =
    DRec { r | store = Dict.empty }


{-| Set a `Bool` value for specified `DRec` field.
-}
setBool : String -> Bool -> DRec -> DRec
setBool field value drec =
    setWith field fromBool value drec


{-| Set a sub `DRec` for spcified `DRec` field.
-}
setDRec : String -> DRec -> DRec -> DRec
setDRec field value drec =
    setWith field fromDRec value drec


{-| Set a `Float` value for specified `DRec` field.
-}
setFloat : String -> Float -> DRec -> DRec
setFloat field value drec =
    setWith field fromFloat value drec


{-| Set a `Int` value for specified `DRec` field.
-}
setInt : String -> Int -> DRec -> DRec
setInt field value drec =
    setWith field fromInt value drec


{-| Set a `Json.Encode.Value` value for specified `DRec` field.
-}
setJson : String -> Json.Encode.Value -> DRec -> DRec
setJson field value drec =
    setWith field fromJson value drec


{-| Set a `String` value for specified `DRec` field.
-}
setString : String -> String -> DRec -> DRec
setString field value drec =
    setWith field fromString value drec


{-| Set a value for specified `DRec` field with a custom value conversion/validation.

In case of an error the value is retained in an internal input buffer and an error
is set for the specified field.

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
setWith : String -> (a -> DField) -> a -> DRec -> DRec
setWith field toValue value (DRec r) =
    let
        setError de =
            { r
                | buffers = Dict.insert field (Basics.toString value) r.buffers
                , errors = Dict.insert field de r.errors
            }
    in
    case
        Dict.get field r.schema
    of
        Nothing ->
            field
                |> UknownField
                |> setError
                |> DRec

        Just dt ->
            if fieldType (toValue value) == dt then
                DRec { r | store = Dict.insert field (toValue value) r.store }
            else
                (Basics.toString (toValue value) ++ " /= " ++ Basics.toString dt)
                    |> TypeMismatch
                    |> setError
                    |> DRec



-- QUERY


{-| For a valid field defined in schema return a value/type mapping.
-}
get : String -> DRec -> Result DError DField
get field (DRec r) =
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
hasSchema : DRec -> Bool
hasSchema (DRec r) =
    not <| Dict.isEmpty r.schema


{-| Check is specified `DRec` contains data.
-}
isEmpty : DRec -> Bool
isEmpty (DRec r) =
    Dict.isEmpty r.store


{-| Query `DRec` schema.

    address : DRec
    address =
        DRec.empty
            |> DRec.field "street_name" DString
            |> DRec.field "building_number" DInt
            |> DRec.field "sub_number" (DMaybe DInt)

    person : DRec
    person =
        DRec.empty
            |> DRec.field "name" DString
            |> DRec.field "address" (DDRec <| DRec.schema address)

-}
schema : DRec -> DSchema
schema (DRec r) =
    DSchema ( r.fields, r.schema )



-- ENCODE


{-| Convert from `Array a` to DField.
-}
fromArray : (a -> DField) -> Array a -> DField
fromArray f v =
    Array.map f v
        |> DArray_


{-| Convert from `Bool` to `DField`.
-}
fromBool : Bool -> DField
fromBool v =
    DBool_ v


{-| Convert from `DRec` to `DField`.
-}
fromDRec : DRec -> DField
fromDRec v =
    DDRec_ v


{-| Convert from `Float` to `DField`.
-}
fromFloat : Float -> DField
fromFloat v =
    DFloat_ v


{-| Convert from `Int` to `DField`.
-}
fromInt : Int -> DField
fromInt v =
    DInt_ v


{-| Convert from `Json.Encode.Value` to `DField`.
-}
fromJson : Json.Encode.Value -> DField
fromJson v =
    DJson_ v


{-| Convert from `List a` to DField.
-}
fromList : (a -> DField) -> List a -> DField
fromList f v =
    List.map f v
        |> DList_


{-| Convert from `Maybe a` to `DField`.
-}
fromMaybe : (a -> DField) -> Maybe a -> DField
fromMaybe f mv =
    case mv of
        Nothing ->
            DMaybe_ Nothing

        Just v ->
            DMaybe_ (Just (f v))


{-| Convert from `String` to `DField`.
-}
fromString : String -> DField
fromString v =
    DString_ v



-- DECODE


{-| Convert from `DField` to `Array a`
-}
toArray : (Result DError DField -> Result DError a) -> Result DError DField -> Result DError (Array a)
toArray toValue rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DArray_ dfarray ->
                    dfarray
                        |> Array.foldr
                            (\df accum ->
                                toValue (Ok df)
                                    |> Result.map (\v -> v :: accum)
                                    |> Result.withDefault accum
                            )
                            []
                        |> Array.fromList
                        |> Ok

                _ ->
                    "toArray"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `Bool`.
-}
toBool : Result DError DField -> Result DError Bool
toBool rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DBool_ v ->
                    Ok v

                DMaybe_ (Just (DBool_ v)) ->
                    Ok v

                _ ->
                    "toBool"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `DRec`.
-}
toDRec : Result DError DField -> Result DError DRec
toDRec rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DDRec_ v ->
                    Ok v

                DMaybe_ (Just (DDRec_ v)) ->
                    Ok v

                _ ->
                    "toDRec"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `Float`.
-}
toFloat : Result DError DField -> Result DError Float
toFloat rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DFloat_ v ->
                    Ok v

                DMaybe_ (Just (DFloat_ v)) ->
                    Ok v

                _ ->
                    "toFloat"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `Int`.
-}
toInt : Result DError DField -> Result DError Int
toInt rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DInt_ v ->
                    Ok v

                DMaybe_ (Just (DInt_ v)) ->
                    Ok v

                _ ->
                    "toInt"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `Json.Encode.Value`.
-}
toJson : Result DError DField -> Result DError Json.Encode.Value
toJson rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DJson_ v ->
                    Ok v

                DMaybe_ (Just (DJson_ v)) ->
                    Ok v

                _ ->
                    "toJson"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `List a`.
-}
toList : (Result DError DField -> Result DError a) -> Result DError DField -> Result DError (List a)
toList toValue rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DList_ dfl ->
                    dfl
                        |> List.foldr
                            (\df accum ->
                                toValue (Ok df)
                                    |> Result.map (\v -> v :: accum)
                                    |> Result.withDefault accum
                            )
                            []
                        |> Ok

                _ ->
                    "toArray"
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
            |> DRec.toMaybe DRec.toString

-}
toMaybe : (Result DError DField -> Result DError a) -> Result DError DField -> Result DError (Maybe a)
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
                                |> Result.map Just
                                |> Result.withDefault Nothing
                                |> Ok

                _ ->
                    "toMaybe"
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField` to `String`.
-}
toString : Result DError DField -> Result DError String
toString rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DString_ v ->
                    Ok v

                DMaybe_ (Just (DString_ v)) ->
                    Ok v

                _ ->
                    "toString"
                        |> TypeMismatch
                        |> Err



-- JSON interop


{-| @private
-}
arrayDecoder : String -> DRec -> (a -> DField) -> Decoder (Array a) -> Decoder DRec
arrayDecoder fname drec toItem decoder =
    decoder
        |> Json.Decode.field fname
        |> Json.Decode.map (\v -> setWith fname (fromArray toItem) v drec)


{-| @private
-}
listDecoder : String -> DRec -> (a -> DField) -> Decoder (List a) -> Decoder DRec
listDecoder fname drec toItem decoder =
    decoder
        |> Json.Decode.field fname
        |> Json.Decode.map (\v -> setWith fname (fromList toItem) v drec)


{-| @private
-}
maybeDecoder : String -> DRec -> (a -> DField) -> Decoder (Maybe a) -> Decoder DRec
maybeDecoder fname drec toItem decoder =
    decoder
        |> Json.Decode.field fname
        |> Json.Decode.map (\v -> setWith fname (fromMaybe toItem) v drec)


{-| @private
-}
fieldDecoder : String -> DType -> DRec -> Decoder DRec
fieldDecoder fname dtype drec =
    case dtype of
        DNever ->
            Json.Decode.fail "DNever is never decoded."

        DArray dvalue ->
            case dvalue of
                VNil ->
                    Json.Decode.fail "DArray VNil is never decoded."

                VBool ->
                    Json.Decode.array Json.Decode.bool
                        |> arrayDecoder fname drec fromBool

                VDRec (DSchema ( forder, stypes )) ->
                    let
                        subRec =
                            DRec
                                { buffers = Dict.empty
                                , errors = Dict.empty
                                , fields = forder
                                , schema = stypes
                                , store = Dict.empty
                                }
                    in
                    Json.Decode.array (subDecoder subRec)
                        |> arrayDecoder fname drec fromDRec

                VFloat ->
                    Json.Decode.array Json.Decode.float
                        |> arrayDecoder fname drec fromFloat

                VInt ->
                    Json.Decode.array Json.Decode.int
                        |> arrayDecoder fname drec fromInt

                VJson ->
                    Json.Decode.array Json.Decode.value
                        |> arrayDecoder fname drec fromJson

                VString ->
                    Json.Decode.array Json.Decode.string
                        |> arrayDecoder fname drec fromString

        DBool ->
            Json.Decode.field fname Json.Decode.bool
                |> Json.Decode.map (\v -> setWith fname fromBool v drec)

        DDRec (DSchema ( forder, stypes )) ->
            let
                subRec =
                    DRec
                        { buffers = Dict.empty
                        , errors = Dict.empty
                        , fields = forder
                        , schema = stypes
                        , store = Dict.empty
                        }
            in
            Json.Decode.field fname (subDecoder subRec)
                |> Json.Decode.map (\v -> setWith fname fromDRec v drec)

        DFloat ->
            Json.Decode.field fname Json.Decode.float
                |> Json.Decode.map (\v -> setWith fname fromFloat v drec)

        DInt ->
            Json.Decode.field fname Json.Decode.int
                |> Json.Decode.map (\v -> setWith fname fromInt v drec)

        DJson ->
            Json.Decode.field fname Json.Decode.value
                |> Json.Decode.map (\v -> setWith fname fromJson v drec)

        DList dvalue ->
            case dvalue of
                VNil ->
                    Json.Decode.fail "DList VNil is never decoded."

                VBool ->
                    Json.Decode.list Json.Decode.bool
                        |> listDecoder fname drec fromBool

                VDRec (DSchema ( forder, stypes )) ->
                    let
                        subRec =
                            DRec
                                { buffers = Dict.empty
                                , errors = Dict.empty
                                , fields = forder
                                , schema = stypes
                                , store = Dict.empty
                                }
                    in
                    Json.Decode.list (subDecoder subRec)
                        |> listDecoder fname drec fromDRec

                VFloat ->
                    Json.Decode.list Json.Decode.float
                        |> listDecoder fname drec fromFloat

                VInt ->
                    Json.Decode.list Json.Decode.int
                        |> listDecoder fname drec fromInt

                VJson ->
                    Json.Decode.list Json.Decode.value
                        |> listDecoder fname drec fromJson

                VString ->
                    Json.Decode.list Json.Decode.string
                        |> listDecoder fname drec fromString

        DMaybe dvalue ->
            case dvalue of
                VNil ->
                    Json.Decode.fail "DMaybe VNil is never decoded."

                VBool ->
                    Json.Decode.maybe Json.Decode.bool
                        |> maybeDecoder fname drec fromBool

                VDRec (DSchema ( forder, stypes )) ->
                    let
                        subRec =
                            DRec
                                { buffers = Dict.empty
                                , errors = Dict.empty
                                , fields = forder
                                , schema = stypes
                                , store = Dict.empty
                                }
                    in
                    Json.Decode.maybe (subDecoder subRec)
                        |> maybeDecoder fname drec fromDRec

                VFloat ->
                    Json.Decode.maybe Json.Decode.float
                        |> maybeDecoder fname drec fromFloat

                VInt ->
                    Json.Decode.maybe Json.Decode.int
                        |> maybeDecoder fname drec fromInt

                VJson ->
                    Json.Decode.maybe Json.Decode.value
                        |> maybeDecoder fname drec fromJson

                VString ->
                    Json.Decode.maybe Json.Decode.string
                        |> maybeDecoder fname drec fromString

        DString ->
            Json.Decode.field fname Json.Decode.string
                |> Json.Decode.map (\v -> setWith fname fromString v drec)


{-| @private
Aggregate `DRec` member decoders.
-}
subDecoder : DRec -> Decoder DRec
subDecoder (DRec r) =
    r.fields
        |> List.foldl
            (\fname accum ->
                Dict.get fname r.schema
                    |> Maybe.map (\dtype -> Json.Decode.andThen (fieldDecoder fname dtype) accum)
                    |> Maybe.withDefault accum
            )
            (DRec r |> Json.Decode.succeed)


{-| Create decoder for specified `DRec`.
-}
decoder : DRec -> Decoder DRec
decoder (DRec r) =
    if Dict.isEmpty r.errors then
        subDecoder (DRec r)
    else
        errorMessage (DRec r)
            |> Maybe.map Json.Decode.fail
            |> Maybe.withDefault
                ("decoder logic failure, how to got here?"
                    |> Json.Decode.fail
                )


{-| Initialize `DRec` data by decoding specified JSON (`Json.Encode.Value`) accordingly to `DRec` schema.
-}
decodeValue : DRec -> Json.Encode.Value -> Result DError DRec
decodeValue drec json =
    if hasSchema drec then
        Json.Decode.decodeValue (decoder drec) json
            |> Result.mapError DecodingFailed
    else
        Err NoSchema


{-| Initialize `DRec` data by decoding specified JSON string literal accordingly to `DRec` schema.
-}
decodeString : DRec -> String -> Result DError DRec
decodeString drec json =
    if hasSchema drec then
        Json.Decode.decodeString (decoder drec) json
            |> Result.mapError DecodingFailed
    else
        Err NoSchema


{-| Encode `DRec` into a JSON object accordingly to `DRec` schema.
-}
encoder : DRec -> Json.Encode.Value
encoder (DRec r) =
    if Dict.isEmpty r.errors then
        subObject (DRec r)
    else
        Json.Encode.object []


{-| @private
Encode specified `DRec`.
-}
subObject : DRec -> Json.Encode.Value
subObject (DRec r) =
    r.fields
        |> List.foldr
            (\field accum ->
                Dict.get field r.store
                    |> Maybe.map (objectField field accum)
                    |> Maybe.withDefault accum
            )
            []
        |> Json.Encode.object


{-| @private
-}
objectField : String -> List ( String, Json.Encode.Value ) -> DField -> List ( String, Json.Encode.Value )
objectField field accum dfield =
    case dfield of
        DArray_ dfarray ->
            let
                valueArray =
                    dfarray
                        |> Array.foldr
                            (\df accum ->
                                case objectField field [] df |> List.head of
                                    Nothing ->
                                        accum

                                    Just ( _, v ) ->
                                        v :: accum
                            )
                            []
                        |> Array.fromList
            in
            ( field, Json.Encode.array valueArray ) :: accum

        DBool_ b ->
            ( field, Json.Encode.bool b ) :: accum

        DDRec_ sdrec ->
            ( field, subObject sdrec ) :: accum

        DFloat_ f ->
            ( field, Json.Encode.float f ) :: accum

        DInt_ i ->
            ( field, Json.Encode.int i ) :: accum

        DJson_ v ->
            ( field, v ) :: accum

        DList_ dfl ->
            let
                valueList =
                    dfl
                        |> List.foldr
                            (\df accum ->
                                case objectField field [] df |> List.head of
                                    Nothing ->
                                        accum

                                    Just ( _, v ) ->
                                        v :: accum
                            )
                            []
            in
            ( field, Json.Encode.list valueList ) :: accum

        DMaybe_ mv ->
            case mv of
                Nothing ->
                    accum

                Just df ->
                    objectField field accum df

        DString_ s ->
            ( field, Json.Encode.string s ) :: accum
