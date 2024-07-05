module Shared exposing
    ( Flags
    , Model, Msg
    , init, update, subscriptions
    , decoder, setToastMsg
    )

{-|

@docs Flags
@docs Model, Msg
@docs init, update, subscriptions

-}

import Browser.Events
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Util.Toast as Toast



-- FLAGS
-- INIT


type alias Model =
    Shared.Model.Model


type alias Flags =
    {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed {}


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    ( { toasts = [], nextToastId = 0 }, Effect.none )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        ToastMsg toastId toastMsg ->
            let
                newToasts =
                    model.toasts
                        |> List.filterMap
                            (\toast ->
                                if Toast.getId toast == toastId then
                                    case Toast.update toastMsg toast of
                                        Toast.UpdateState newToast ->
                                            Just newToast

                                        Toast.Close ->
                                            Nothing

                                else
                                    Just toast
                            )
            in
            ( { model | toasts = newToasts }, Effect.none )

        ShowToast toast ->
            ( { model
                | toasts = Toast.setId ("main-app-toast-" ++ String.fromInt model.nextToastId) toast :: model.toasts
                , nextToastId = model.nextToastId + 1
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.batch
        [ model.toasts
            |> List.map
                (\toast ->
                    Toast.subscriptions toast
                        |> Sub.map (ToastMsg (Toast.getId toast))
                )
            |> Sub.batch
        ]


setToastMsg : String -> Toast.Msg -> Msg
setToastMsg string msg =
    ToastMsg string msg
