module Main exposing (main)

import Browser
import Html exposing (Html, button, div, span, text, textarea)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import List exposing (map)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { input : String, nextId : Int, tweets : List Tweet }


type alias Id =
    Int


type Tweet
    = Tweet
        { id : Id
        , content : String
        , icon : String
        , displayName : String
        , accountName : String
        , liked : Bool
        }


initialModel : Model
initialModel =
    { input = ""
    , nextId = 3
    , tweets =
        [ Tweet
            { id = 1
            , icon = ""
            , displayName = "もろこし太郎"
            , accountName = "morokoshi"
            , content = "今日も1日もろこしがうまい"
            , liked = False
            }
        , Tweet
            { id = 2
            , icon = ""
            , displayName = "エビデンス"
            , accountName = "evidence"
            , content = "かにみそたべたい"
            , liked = False
            }
        ]
    }


newTweet : Id -> String -> Tweet
newTweet id s =
    Tweet
        { id = id
        , icon = ""
        , displayName = "ミスター死"
        , accountName = "mrdeath"
        , content = s
        , liked = False
        }


toggleLike : Tweet -> Tweet
toggleLike (Tweet tweet) =
    Tweet { tweet | liked = not tweet.liked }


type Msg
    = ToggleLike Int
    | Input String
    | SendTweet


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLike tweetId ->
            { model
                | tweets =
                    map
                        (\((Tweet { id }) as t) ->
                            if id == tweetId then
                                toggleLike t

                            else
                                t
                        )
                        model.tweets
            }

        Input s ->
            { model | input = s }

        SendTweet ->
            { model
                | input = ""
                , nextId = model.nextId + 1
                , tweets = newTweet model.nextId model.input :: model.tweets
            }


view : Model -> Html Msg
view model =
    div [] (viewNewTweet model.input :: map viewTweet model.tweets)


viewTweet : Tweet -> Html Msg
viewTweet (Tweet { id, content, icon, displayName, accountName, liked }) =
    div [ class "tweet" ]
        [ div [ class "icon-container" ] [ text icon ]
        , div [ class "body-container" ]
            [ div [ class "status-display" ]
                [ span [ class "display-name" ] [ text displayName ]
                , span [ class "account-name" ] [ text ("@" ++ accountName) ]
                ]
            , div [ class "content" ] [ text content ]
            , div [ class "status-icon", onClick (ToggleLike id) ]
                [ text
                    (if liked then
                        "♥"

                     else
                        "♡"
                    )
                ]
            ]
        ]


viewNewTweet : String -> Html Msg
viewNewTweet input =
    div []
        [ textarea [ class "tweet-textarea", onInput Input, value input ] []
        , button [ class "send-tweet", onClick SendTweet ] [ text "Tweet" ]
        ]
