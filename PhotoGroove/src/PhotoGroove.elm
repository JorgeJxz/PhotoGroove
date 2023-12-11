module PhotoGroove exposing (main)

import Html exposing (div,h1,img,text)
import Html.Attributes exposing (..)

view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [id "thumbnails" ]
            [ img [ src "https://picsum.photos/200/300?random=1"] []
            , img [ src "https://picsum.photos/200/300?random=2"] []
            , img [ src "https://picsum.photos/200/300?random=3"] []
            ]
        ]

main  =
    view "no model yet"