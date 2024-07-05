module Shared.Msg exposing (Msg(..))

import Util.Toast as Toast exposing (Toast)


type Msg
    = ToastMsg String Toast.Msg
    | ShowToast Toast
