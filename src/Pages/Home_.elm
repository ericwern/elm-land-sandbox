module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Html.Events as E
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Util.Toast as Toast
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = Toast


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Toast ->
            ( model
            , Effect.toast
                (Toast.init
                    |> Toast.setTitle "Toast title"
                    |> Toast.setContent
                        (Html.div [] [ Html.text "This is the toast content" ])
                )
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        toStr field =
            field shared |> String.fromInt
    in
    { title = "Pages.Home_"
    , body =
        [ Html.div [] [ Html.text "This is the body of Pages.Home_" ]
        , Html.button [ E.onClick Toast ] [ Html.text "Show toast" ]
        ]
    }
