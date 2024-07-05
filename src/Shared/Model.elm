module Shared.Model exposing (Model)

import Util.Toast exposing (Toast)


type alias Model =
    { toasts : List Toast
    , nextToastId : Int
    }
