module Fetch exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, string, map3, int)



-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- USER
type alias User =
  {
      id: Int
      ,name: String
      ,score: Int
  }

type alias ListModel =
  {
    userList: List User
  , msg: String
  }

-- MODEL
type Model
  = Failure
  | Loading
  | Success User
  | SuccessAll (List User)


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getSingleUser)

-- UPDATE
type Msg
  = MorePlease
  | SingleUserButton
  | DeleteUser
  | GetAllUsersButton
  | GotUser (Result Http.Error User)
  | GotAllUserList (Result Http.Error (List User))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getSingleUser)

    SingleUserButton ->
      (Loading, getSingleUser)

    DeleteUser ->
      (Loading, deleteUser)
    
    GetAllUsersButton ->
      (Loading, getAllUsers)

    GotUser result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

    GotAllUserList result ->
      case result of
        Ok url ->
          (SuccessAll url, Cmd.none)
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
    , hr [] []
    , searchUser model
    , viewTable model
    ]

searchUser : Model -> Html Msg
searchUser model = 
    div []
        [ 
        h5 [] [text "Fetch all users here: "]
        , button [ onClick GetAllUsersButton] [ text "Fetch All users!" ]
        , hr [] []
        , h5 [] [text "Fetch / Delete user here: "]
        , input [type_ "text", placeholder "search with id" ] []
        , br [] []
        , button [ onClick SingleUserButton] [ text "Fetch user!" ]
        , button [ onClick DeleteUser] [ text "Delete user!" ]
        , hr [] []
        , h5 [] [text "Create a user here: "]
        , input [type_ "text", placeholder "user name" ] []
        , input [type_ "text", placeholder "user score" ] []
        , br [] []
        , button [ onClick MorePlease] [ text "Create user!" ]
        , hr [] []
        ]

viewTable : Model -> Html Msg
viewTable model =
  case model of
    Failure ->
      div []
        [ text "I could not load any users. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success user ->
      div []
        [ 
        h3 [] [text "Table of users"]
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
                    td[] [text (String.fromInt user.id)]
                    ,td[] [text user.name]
                    ,td[] [text (String.fromInt user.score)]
                ]
            ]
        ]
    
    SuccessAll list ->
      table []
        ([ tr []
            [ th [] [ text "Id" ]
            , th [] [ text "Name" ]
            , th [] [ text "Score"]
            ]
         ]
            ++ List.map showUser list
        )

showUser: User -> Html Msg
showUser user =
    tr [] 
        [
            td[] [text (String.fromInt user.id)]
            ,td[] [text user.name]
            ,td[] [text (String.fromInt user.score)]
        ]

-- HTTP
deleteUser : Cmd Msg
deleteUser = 
      Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:4712/member/7"
        , body = Http.emptyBody
        , expect = Http.expectJson GotUser userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
        
getSingleUser : Cmd Msg
getSingleUser = 
    Http.get
        { url = "http://localhost:4712/member/7"
        , expect = Http.expectJson GotUser userDecoder
        }

getAllUsers : Cmd Msg
getAllUsers =
    Http.get
        { url = "http://localhost:4712/member"
        , expect = Http.expectJson GotAllUserList userListDecoder
        }

-- JSON DECODERS

userDecoder: Decoder User
userDecoder = map3 User (field "id" int) (field "name" string) (field "score" int)

userListDecoder: Decoder ( List User )
userListDecoder = JD.list userDecoder
  

msgDecoder : Decoder String
msgDecoder = 
  field "name" string
