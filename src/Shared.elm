module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Gen.Route
import Json.Decode as Json
import Request exposing (Request)
import Storage exposing (Storage)


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage
    }

type Msg
    = StorageUpdated Storage

init : Request -> Flags -> ( Model, Cmd Msg )
init req flags =
    let
        model =  { storage = Storage.storageFromJson flags }
    in
    ( model
    , if model.storage.credentials /= Nothing && req.route == Gen.Route.Login then
        Request.replaceRoute Gen.Route.Home_ req
      else if req.route == Gen.Route.Logout then
        Request.replaceRoute Gen.Route.Login req
      else
        Cmd.none
    )

update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage ->
            ( { model | storage = storage }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange StorageUpdated