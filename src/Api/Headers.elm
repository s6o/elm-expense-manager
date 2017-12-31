module Api.Headers
    exposing
        ( objectHeader
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


{-| Create header for JWT token.
-}
tokenHeader : Maybe String -> List ( String, String )
tokenHeader token =
    token
        |> Maybe.map (\t -> [ ( "Authorization", "Bearer " ++ t ) ])
        |> Maybe.withDefault []
