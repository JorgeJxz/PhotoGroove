module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Array exposing (Array)

urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"

type Msg 
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

--Tipos personalizados, y las variantes, que son los valores que puede tener el tipo
type ThumbnailSize 
 = Small
 | Medium
 | Large

{- Podemos annotate functions escribiendo "->" entre sus argumentos y valores de retorno
ademas "->" forma parte de la creacion de funciones anonimas \w h -> w * h 
-}
view : Model -> Html Msg    
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me!"]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url) ]
        []

viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size)] []
        , text (sizeToString size)
        ]

{- pag 76 (POR HACER) *****************************************************************************************
  ~ make the Medium option display as selected on page load
  ~ a broader event handler than onClick—one that detects whenever the radio state changes, even if it not from a click.
-}       


-- pag 70 Crear la funcion sizeToClass (POR HACER) ************************************************************
sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        
        Medium ->
            "med"

        Large ->
            "large"

type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }

initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Large
    }

photoArray : Array Photo
photoArray = 
    Array.fromList initialModel.photos

{- Esta funcion trabaja con Just y Nothing, los valores que puede tener Maybe 
type Maybe value
 = Just value
 | Nothing
 Pag 73
-}
getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url
        Nothing ->
            ""

update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSize size ->
            { model | chosenSize = size}
        
        ClickedSurpriseMe -> 
            { model | selectedUrl = "3.jpeg" }
       
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


 {- ANOTACIONES GENERALES

    ~ "_ ->" es la rama default de los case, asi que como con un else
    nos aseguramos que pase algo siempre
        
-}