import Html exposing (..)
import Html.Attributes exposing (placeholder, value, type_, class, href)
import Html.Events exposing (..)
import Http
import Generated exposing (..)
import Result
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  {
    users: List (User)
  , newUserFirstName: String
  , newUserLastName: String
  }


init : (Model, Cmd Msg)
init =
  ( Model [] "" ""
  , fetchUsers
  )


type Msg
  = FetchUsers
  | SetUsers (Result Http.Error (List User))
  | SetNewUserFirstName String
  | SetNewUserLastName String
  | CreateUser


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    FetchUsers ->
      ( model, fetchUsers )

    SetUsers newUsers ->
      pure { model | users = Result.withDefault model.users newUsers }

    SetNewUserFirstName name ->
      pure { model | newUserFirstName = name }

    SetNewUserLastName name ->
      pure { model | newUserLastName = name }

    CreateUser ->
      if validate model then
        ( { model | newUserFirstName = "", newUserLastName = "" }
        , postApiUsersCreate
            { firstName = model.newUserFirstName
            , lastName  = model.newUserLastName
            }
            |> Http.send (\_ -> FetchUsers)
        )
      else
        pure model

validate : Model -> Bool
validate { newUserFirstName, newUserLastName } =
  List.all (not << String.isEmpty) [ newUserFirstName, newUserLastName ]

view : Model -> Html.Html Msg
view model =
  Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [
                  h1 [] [text "Users"]
                , viewUserForm model
                , viewUserList model
                , a [href "/docs"] [ text "API docs" ]
                ]
            ]

        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewUserForm : Model -> Html.Html Msg
viewUserForm model =
  Grid.row []
    [ Grid.col []
        [ h2 [] [ text "Create a user" ]
        , Form.formInline [ onSubmit CreateUser ]
            [ Form.group []
                [ Input.text
                    [ Input.placeholder "First name"
                    , Input.value model.newUserFirstName
                    , Input.onInput SetNewUserFirstName
                    ]
                ]
            , Form.group []
                [ Input.text
                    [ Input.placeholder "Last name"
                    , Input.value model.newUserLastName
                    , Input.onInput SetNewUserLastName
                    ]
                ]
            , Button.button [ Button.primary ] [ text "Create user" ]
            ]
        ]
    ]


viewUserList : { a | users : List User } -> Html Msg
viewUserList model =
  Grid.row []
    [ Grid.col []
        [ h2 [] [ text "All users" ]
        , Grid.row []
            (List.map viewUser model.users)
        , Button.button
            [ Button.onClick FetchUsers ]
            [ text "Refresh user list" ]
        ]
    ]

viewUser : User -> Grid.Column Msg
viewUser user =
  Grid.col []
    [ text (user.firstName ++ " " ++ user.lastName)
    ]


fetchUsers : Cmd Msg
fetchUsers = getApiUsers |> Http.send SetUsers

pure : a -> ( a, Cmd b )
pure model =
  ( model, Cmd.none )
