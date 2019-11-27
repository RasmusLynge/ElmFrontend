module Fetch exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL
type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getRandomCatGif)



-- UPDATE
type Msg
  = MorePlease
  | GotGif (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomCatGif)

    GotGif result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Learning to fetch" ]
    , searchUser model
    , viewGif model
    ]

searchUser : Model -> Html Msg
searchUser model = 
    div []
        [ 
        h5 [] [text "Fetch all users here: "]
        , button [ onClick MorePlease] [ text "Fetch All users!" ]
        , h5 [] [text "Fetch / Delete user here: "]
        , input [type_ "text", placeholder "search with id" ] []
        , br [] []
        , button [ onClick MorePlease] [ text "Fetch user!" ]
        , button [ onClick MorePlease] [ text "Delete user!" ]
        , h5 [] [text "Create a user here: "]
        , input [type_ "text", placeholder "user name" ] []
        , input [type_ "text", placeholder "user score" ] []
        , br [] []
        , button [ onClick MorePlease] [ text "Create user!" ]
        , br [] []
        , br [] []
        , h3 [] [text "Table of users"]
        , table [] 
            [
            tr []
                [
                    th[] [text "id "]
                    , th[] [text "name "]
                    , th[] [text "score "]
                ]
            ,tr [] 
                [
                    td[] [text "1"]
                    ,td[] [text "Rasmus"]
                    ,td[] [text "+9000"]
                ]
            ,tr [] 
                [
                    td[] [text "2"]
                    ,td[] [text "Magnus"]
                    ,td[] [text "13"]
                ]
            ]
        ]

viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random cat for some reason. "
        , button [ onClick MorePlease ] [ text "Try Cats Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success url ->
      Debug.log(url)
      div []
        [ 
        br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , br [] []
        , button [ onClick MorePlease, style "display" "block" ] [ text "More Cats Please!" ]
        , img [ src url ] []
        ]



-- HTTP
getRandomCatGif : Cmd Msg
getRandomCatGif =
  Http.get
    { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
    , expect = Http.expectJson GotGif gifDecoder
    }


gifDecoder : Decoder String
gifDecoder =
  field "data" (field "image_url" string)