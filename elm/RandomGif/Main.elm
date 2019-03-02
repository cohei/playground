module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h2, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode

main : Program () Model Msg
main =
    Browser.element { init = \_ -> init, update = update, subscriptions = \_ -> Sub.none, view = view }


type alias Model =
    { topic : String, gifUrl : String }


init : ( Model, Cmd Msg )
init =
    ( Model "cats" "waiting.gif", Cmd.none )


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( model, getRandomGif model.topic )

        NewGif (Err _) ->
            ( model, Cmd.none )

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.topic ]
        , img [ src model.gifUrl ] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
    Http.send NewGif request


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string
