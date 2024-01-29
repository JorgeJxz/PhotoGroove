module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Random
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)

urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"

type Msg 
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


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
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage)]
        

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise me!"]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
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
    { url : String
    , size : Int
    , title : String
    }

photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"

-- buildPhoto : String -> Int -> String -> Photo
-- buildPhoto url size title =
--     { url = url, size = size, title = title}

type Status 
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }

initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status}, Cmd.none)

        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size}, Cmd.none )
       
        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
                
                Loaded [] _->
                    ( model, Cmd.none )
                
                Loading ->
                    ( model, Cmd.none)

                Errored errorMessage ->
                    ( model, Cmd.none)

        GotPhotos (Ok responseStr) ->
                    case String.split "," responseStr of
                        (firstUrl :: _) as urls ->
                            let
                                photos = 
                                    List.map Photo urls
                            in
                            ( { model | status = Loaded photos firstUrl }, Cmd.none)

        GotPhotos (Err _) ->
            ( model, Cmd.none)

selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _->
            Loaded photos url

        Loading ->
            status thought
        
        Errored errorMessage ->
            status

initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GotPhotos
        }
       
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


 {- ANOTACIONES GENERALES

    ~ "_ ->" es la rama default de los case, asi que como con un else
    nos aseguramos que pase algo siempre

    ~ ver apendice B para mas info sobre el random y modulos en general

    ~ (revisar) The () value is known as unit. It contains no information whatsoever. It’s both a value and a type; the () type can be satisfied only with
        the () value. We could use it to write a function like this:
        getUltimateAnswer : () -> Int
        getUltimateAnswer unit =
        40 + 2
        
    ~ Investigar  list zipper

    ~<| el pana aqui lo que hace es que se pueda llamar a una funcion sin parentesis

        String.toUpper (String.reverse "hello")
                        =
        String.toUpper <| String.reverse "hello"

    ~ Esta funcion trabaja con Just y Nothing, los valores que puede tener Maybe 
    type Maybe value
    = Just value
    | Nothing
    Pag 73

    getPhotoUrl : Int -> String
    getPhotoUrl index =
        case Array.get index photoArray of
            Just photo ->
                photo.url
            Nothing ->
                ""

    randomPhotoPicker : Random.Generator Int
    randomPhotoPicker =
        Random.int 0 (Array.length photoArray - 1)

    ~Random.uniform function!
    Random.uniform : elem -> List elem -> Random.Generator elem

    ~THE :: PATTERN
    Loaded (firstPhoto :: otherPhotos) _ -> entiendo que sirve para separar parametros obligatorios
    de los opcionales. INVESTIGAR

    ~THE [ ] PATTERN
    Loaded [] _ ->
        ( model, Cmd.none )
    This pattern will match Loaded variants where the List Photo value is empty. It says
    that if the user clicks Surprise Me! and we loaded zero photos, then the Surprise Me!
    button does nothing. Thanks to this change, our code compiles again!

        
-}