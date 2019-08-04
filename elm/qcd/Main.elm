module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onCheck)
import List
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type Restriction
    = Cost
    | Deliver
    | Quality


limit : Int
limit =
    2


overLimit : Int -> Bool
overLimit n =
    n > limit


type alias Model =
    List Restriction


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )


type Msg
    = Toggle Restriction Bool
    | OverLimit Restriction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle r False ->
            ( off r model, Cmd.none )

        Toggle r True ->
            ( on r model, when (List.length >> (+) 1 >> overLimit) (choose OverLimit) model )

        OverLimit r ->
            ( off r model, Cmd.none )


off : Restriction -> Model -> Model
off restriction model =
    List.filter ((/=) restriction) model


on : Restriction -> Model -> Model
on restriction model =
    restriction :: model


when : (a -> Bool) -> (a -> Cmd msg) -> a -> Cmd msg
when p f x =
    if p x then
        f x

    else
        Cmd.none


choose : (a -> msg) -> List a -> Cmd msg
choose f =
    uncons
        >> Maybe.map (\( x, xs ) -> Random.generate f <| Random.uniform x xs)
        >> Maybe.withDefault Cmd.none


uncons : List a -> Maybe ( a, List a )
uncons xs =
    List.head xs |> Maybe.andThen (\y -> List.tail xs |> Maybe.map (\ys -> ( y, ys )))


view : Model -> Html Msg
view model =
    div []
        [ label [ blockDisploy ]
            [ restrictionCheckbox model Cost
            , text "予算"
            ]
        , label [ blockDisploy ]
            [ restrictionCheckbox model Deliver
            , text "納期"
            ]
        , label [ blockDisploy ]
            [ restrictionCheckbox model Quality
            , text "品質"
            ]
        ]


restrictionCheckbox : Model -> Restriction -> Html Msg
restrictionCheckbox model r =
    input [ type_ "checkbox", checked (List.member r model), onCheck (Toggle r) ] []


blockDisploy =
    style "display" "block"
