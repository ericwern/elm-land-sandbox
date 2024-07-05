module Util.Toast exposing
    ( Msg
    , Style(..)
    , Toast
    , Update(..)
    , getId
    , init
    , setContent
    , setDisplayDuration
    , setFadeOutDuration
    , setId
    , setStyle
    , setTitle
    , subscriptions
    , update
    , view
    , viewList
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Time


ticksPerSecond : Int
ticksPerSecond =
    10


{-| The internal state of a toast.

This is an opaque type to give this module full control over all changes

-}
type Toast
    = Toast
        { id : String
        , timer : Int
        , fadeOutDuration : Int
        , title : String
        , content : Html Never
        , attributes : List (H.Attribute Msg)
        , style : Style
        }


{-| Toast's internal msg type
-}
type Msg
    = CountDown
    | ClickClose


{-| An update action describing what just happened to the toast

This is the return value to `update`, and should be cased on in order to figure
out what to do with the updated toast (remove it or set the new state)

-}
type Update
    = UpdateState Toast
    | Close


{-| Specifies which style a toast should use (success or failure)

Should be set to determine what kind of style to use for the toast
Default - 'Success'

-}
type Style
    = Success
    | Failure


{-| Perform an update on a toast, given a message

The return value of this call should be cased to decide whether to remove
(Close) or update the toast's state (UpdateState)

-}
update : Msg -> Toast -> Update
update msg (Toast toast) =
    case msg of
        CountDown ->
            if toast.timer == 0 then
                Close

            else
                UpdateState (Toast { toast | timer = toast.timer - 1 })

        ClickClose ->
            UpdateState (Toast { toast | timer = fadeOutTicks (Toast toast) })


{-| Initialize a new toast
-}
init : Toast
init =
    Toast
        { id = ""
        , timer = 60
        , fadeOutDuration = 500
        , title = ""
        , content = H.text ""
        , attributes = []
        , style = Success
        }


{-| Set the title of a toast
-}
setTitle : String -> Toast -> Toast
setTitle title (Toast toast) =
    Toast { toast | title = title }


{-| Set the content of a toast
-}
setContent : Html Never -> Toast -> Toast
setContent content (Toast toast) =
    Toast { toast | content = content }


{-| Set how long it takes, in milliseconds, for a toast to fade out

This is the amount of milliseconds it takes from a toast begins to fade out
until it is completely gone. The fade-out duration overlaps with the display
duration, and so the display duration means the total duration the toast is
visible in any capacity on the screen.

-}
setFadeOutDuration : Int -> Toast -> Toast
setFadeOutDuration fadeOutDuration (Toast toast) =
    Toast { toast | fadeOutDuration = fadeOutDuration }


{-| Set how long, in milliseconds, a toast will be visible on the screen.

This overlaps with the fade-out duration, and so the display duration means the
total duration the toast is visible in any capacity on the screen.

-}
setDisplayDuration : Int -> Toast -> Toast
setDisplayDuration displayDurationMillis (Toast toast) =
    Toast { toast | timer = (displayDurationMillis * ticksPerSecond) // 1000 }


{-| Set the ID of a toast

It is generally useful to always set a unique ID on all toasts. Toasts without
IDs don't play that well with Elm's virtual DOM and runtime.

-}
setId : String -> Toast -> Toast
setId id (Toast toast) =
    Toast { toast | id = id }


{-| Set the style of a toast
-}
setStyle : Style -> Toast -> Toast
setStyle style (Toast toast) =
    Toast { toast | style = style }


{-| Get the ID of a toast
-}
getId : Toast -> String
getId (Toast toast) =
    toast.id


fading : Toast -> Bool
fading (Toast toast) =
    toast.timer <= fadeOutTicks (Toast toast)


fadeOutTicks : Toast -> Int
fadeOutTicks (Toast toast) =
    (toast.fadeOutDuration * ticksPerSecond) // 1000


{-| Render a toast
-}
view : Toast -> Html Msg
view (Toast toast) =
    let
        fadingAttributes =
            if fading (Toast toast) then
                [ A.style "transition-duration" (String.fromInt toast.fadeOutDuration ++ "ms")
                , A.style "transition-property" "opacity"
                , A.style "opacity" "0"
                ]

            else
                []
    in
    case toast.style of
        Success ->
            H.div (A.id toast.id :: fadingAttributes)
                [ H.div
                    (A.class "toast_success toast"
                        :: toast.attributes
                        ++ [ A.style "display" "flex"
                           , A.style "position" "relative"
                           , A.style "padding" "10px"
                           , A.style "margin-top" "10px"
                           , A.style "box-shadow" "0px 0px 6px -1px rgba(0, 0, 0, 0.45)"
                           , A.style "background-color" "#EF463CFF"
                           ]
                    )
                    [ H.button [ A.class "close_toast_x", E.onClick ClickClose ] []
                    , H.div [ A.class "toast_icon_success" ] []
                    , H.div [ A.class "toast_content" ]
                        [ H.h4 [ A.class "toast_title" ] [ H.text toast.title ]
                        , H.map never toast.content
                        ]
                    ]
                ]

        Failure ->
            H.div (A.id toast.id :: fadingAttributes)
                [ H.div (A.class "toast_error toast" :: toast.attributes)
                    [ H.button [ A.class "close_toast_x", E.onClick ClickClose ] []
                    , H.div [ A.class "toast_icon_failure" ] []
                    , H.div [ A.class "toast_content" ]
                        [ H.h4 [ A.class "toast_title" ] [ H.text toast.title ]
                        , H.map never toast.content
                        ]
                    ]
                ]


{-| Render a list of toasts in a fixed container on the left side of the screen
-}
viewList : List Toast -> Html ( String, Msg )
viewList toasts =
    H.div
        [ A.class "toast_list_container"
        , A.style "width" "450px"
        , A.style "padding" "10px"
        , A.style "position" "fixed"
        , A.style "bottom" "20px"
        , A.style "left" "120px"
        ]
    <|
        List.map (\toast -> view toast |> H.map (Tuple.pair (getId toast))) toasts


{-| Subscriptions of a toast.

Generally, always use this. Without this, timers and fading will not work.

-}
subscriptions : Toast -> Sub Msg
subscriptions _ =
    Time.every (1000 / toFloat ticksPerSecond) <| always CountDown
