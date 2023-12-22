module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)

urlPrefix : String
urlPrefix =
    "https://picsum.photos/200/300?random="

type alias Msg =
    { description : String, data : String}

view : Model -> Html Msg    
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos) 
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick { description = "ClickedPhoto", data = thumb.url } ]
        []

type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    }

initialModel : Model
initialModel =
    { photos =
        [ { url = "1" }
        , { url = "2" }
        , { url = "3" }
        ]
    , selectedUrl = "1"
    }

photoArray : Array Photo
photoArray = 
    Array.fromList initialModel.photos


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }
    else model

    
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
