import Html exposing (..)
import Html.Attributes exposing (placeholder, value, type_, class, href)
import Html.Events exposing (..)
import Http
import Generated exposing (..)
import Result

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
  div []
    [ div
        [ class "container-fluid" ]
        [ h1 [] [ text "Users" ]
        , viewUserForm model
        , viewUserList model
        ]
    , p []
        [a [href "/docs"] [ text "API docs" ]]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewUserForm : Model -> Html.Html Msg
viewUserForm model =
  div
    [ class "row" ]
    [ div
        [ class "col-lg-12" ]
        [ h2 [] [ text "Create a user" ]
        , form
            [ class "form-inline"
            , onSubmit CreateUser
            ]
            [ div
                [ class "form-group" ]
                [ input
                    [ placeholder "First name"
                    , class "form-control"
                    , value model.newUserFirstName
                    , onInput SetNewUserFirstName
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ input
                    [ placeholder "Last name"
                    , class "form-control"
                    , value model.newUserLastName
                    , onInput SetNewUserLastName
                    ]
                    []
                ]
            , button
                [ type_ "submit"
                , class "btn btn-default"
                ]
                [ text "Create user" ]
            ]
        ]
    ]


viewUserList : { a | users : List User } -> Html Msg
viewUserList model =
  div
    []
    [ h2 [] [ text "All users" ]
    , div
        [ class "row" ]
        (List.map viewUser model.users)
    , button
        [ onClick FetchUsers
        , class "btn btn-default"
        ]
        [ text "Refresh user list" ]
    ]

viewUser : User -> Html.Html Msg
viewUser user =
  div
    [ class "col-lg-3" ]
    [ div
        [ class "panel panel-default" ]
        [ div
            [ class "panel-heading" ]
            [ text (user.firstName ++ " " ++ user.lastName) ]
        ]
    ]


fetchUsers : Cmd Msg
fetchUsers = getApiUsers |> Http.send SetUsers

pure : a -> ( a, Cmd b )
pure model =
  ( model, Cmd.none )
