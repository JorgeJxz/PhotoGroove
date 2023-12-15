module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

urlPrefix =
    "https://picsum.photos/200/300?random="

view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ] (List.map viewThumbnail model)
        ]
viewThumbnail thumb =
    img [ src (urlPrefix ++ thumb.url) ] []
initialModel =
    [ { url = "1" }
    , { url = "2" }
    , { url = "3" }
    ]

main  =
    view initialModel