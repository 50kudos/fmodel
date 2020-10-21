module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Json.Decode as D exposing (Decoder, Value, succeed)
import Json.Decode.Extra as DE
import Json.Encode as E


type alias Sch =
    { sch : Type
    , title : String
    , description : String
    , examples : List D.Value
    }


type Type
    = TRecord RecordFields
    | TList Sch
    | TTuple (List Sch)
    | TUnion (List Sch)
    | TLeaf LeafType
    | TRef String
    | TValue D.Value
    | TAny


type alias RecordFields =
    { fields : Dict String Sch
    , order : List String
    }


type LeafType
    = TString
    | TNumber
    | TInteger
    | TBool
    | TNull


type alias Model =
    { models : List Sch
    }


schsDecoder : Decoder (List Sch)
schsDecoder =
    D.list schDecoder


schDecoder : Decoder Sch
schDecoder =
    D.succeed Sch
        |> DE.andMap (D.oneOf [ tRecord, tList, tTuple, tUnion, tRef, tValue, tAny ])
        |> DE.andMap (D.field "title" D.string |> DE.withDefault "")
        |> DE.andMap (D.field "description" D.string |> DE.withDefault "")
        |> DE.andMap (D.field "examples" (D.list D.value) |> DE.withDefault [])


tRecord : Decoder Type
tRecord =
    D.succeed RecordFields
        |> DE.when (D.field "type" D.string) (\t -> t == "object")
        |> DE.andMap (D.field "properties" (D.dict (D.lazy (\_ -> schDecoder))))
        |> DE.andMap (D.field "order" (D.list D.string))
        |> D.map TRecord


tList : Decoder Type
tList =
    D.succeed TList
        |> DE.when (D.field "type" D.string) (\t -> t == "array")
        |> DE.andMap (D.field "items" (D.lazy (\_ -> schDecoder)))


tTuple : Decoder Type
tTuple =
    D.succeed TTuple
        |> DE.when (D.field "type" D.string) (\t -> t == "array")
        |> DE.andMap (D.field "items" (D.list (D.lazy (\_ -> schDecoder))))


tUnion : Decoder Type
tUnion =
    D.succeed TUnion
        |> DE.andMap (D.field "any_of" (D.list (D.lazy (\_ -> schDecoder))))


tRef : Decoder Type
tRef =
    D.succeed TRef
        |> DE.andMap (D.field "$ref" D.string)


tValue : Decoder Type
tValue =
    D.succeed TValue
        |> DE.andMap (D.field "const" D.value)


tAny : Decoder Type
tAny =
    D.succeed TAny


null : Sch
null =
    { sch = TLeaf TNull
    , title = ""
    , description = ""
    , examples = [ E.null ]
    }


init : D.Value -> ( Model, Cmd Msg )
init json =
    let
        models =
            case D.decodeValue schsDecoder json of
                Ok models_ ->
                    models_

                Err err ->
                    let
                        errMsg =
                            Debug.log "Decode Err" (D.errorToString err)
                    in
                    [ null ]
    in
    ( { models = Debug.log "Decode Ok" models }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



--- View layer ---


type Msg
    = Noop


viewLeaf : LeafType -> Html msg
viewLeaf leaf =
    case leaf of
        TString ->
            text "string"

        TNumber ->
            text "number"

        TInteger ->
            text "integer"

        TBool ->
            text "bool"

        TNull ->
            text "null"


viewFields : RecordFields -> Html msg
viewFields fields =
    div []
        (List.map
            (\k ->
                Dict.get k fields.fields
                    |> Maybe.withDefault null
                    |> mapSch
            )
            fields.order
        )


mapSch : Sch -> Html msg
mapSch sch =
    case sch.sch of
        TRecord recordFields ->
            viewFields recordFields

        TList sch_ ->
            mapSch sch_

        TTuple schs ->
            div [] (List.map mapSch schs)

        TUnion schs ->
            div [] (List.map mapSch schs)

        TLeaf leaf ->
            viewLeaf leaf

        TRef ref ->
            text ref

        TValue json ->
            text "json"

        TAny ->
            text "any"


view : Model -> Html Msg
view model =
    div [] (List.map mapSch model.models)



--- Boundary ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
