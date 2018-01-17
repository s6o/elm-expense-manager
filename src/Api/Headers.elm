module Api.Headers
    exposing
        ( objectHeader
        , recordHeader
        , tokenHeader
        )

{-| Http header helpers
-}


{-| Request PostgREST to return result as JSON object instead of JSON array.
-}
objectHeader : List ( String, String )
objectHeader =
    [ ( "Accept", "application/vnd.pgrst.object+json" )
    ]


{-| Request PostgREST to return the inserted/updated record represenation.
This is useful to avoid another request to get a complete record (with default values).
-}
recordHeader : List ( String, String )
recordHeader =
    [ ( "Prefer", "return=representation" )
    ]


{-| Create header for JWT token.
-}
tokenHeader : Maybe String -> List ( String, String )
tokenHeader token =
    token
        |> Maybe.map (\t -> [ ( "Authorization", "Bearer " ++ t ) ])
        |> Maybe.withDefault []
