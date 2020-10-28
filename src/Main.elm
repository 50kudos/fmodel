port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, classList, id, list, style, type_)
import Html.Events exposing (onClick, preventDefaultOn)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2)
import Json.Decode as D exposing (Decoder, Value, succeed)
import Json.Decode.Extra as DE
import Json.Encode as E


port stateUpdate : (D.Value -> msg) -> Sub msg


port pub : E.Value -> Cmd msg


type alias Fmodel =
    { key : String
    , sch : Sch
    }


type alias Sch =
    { type_ : Type
    , title : String
    , description : String
    , examples : List D.Value
    , anchor : Maybe String
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


type alias SchFile =
    { id : String
    , sch : Sch
    }


type alias AnchorsModels =
    List ( String, String )


type alias Model =
    { currentFile : SchFile
    , anchorsModels : AnchorsModels
    }


mainDecoder : Decoder Model
mainDecoder =
    D.succeed Model
        |> DE.andMap (D.field "currentFile" schFileDecoder)
        |> DE.andMap (D.field "anchorsModels" anchorsModelsDecoder)


anchorsModelsDecoder : Decoder AnchorsModels
anchorsModelsDecoder =
    D.succeed Tuple.pair
        |> DE.andMap (D.index 0 D.string)
        |> DE.andMap (D.index 1 D.string)
        |> D.list


schFileDecoder : Decoder SchFile
schFileDecoder =
    D.succeed SchFile
        |> DE.andMap (D.field "id" D.string)
        |> DE.andMap (D.field "schema" schDecoder)


schsDecoder : Decoder (List Fmodel)
schsDecoder =
    D.dict schDecoder
        |> D.map Dict.toList
        |> D.map (\fmodels -> List.map (\( key, sch ) -> Fmodel key sch) fmodels)


schDecoder : Decoder Sch
schDecoder =
    D.succeed Sch
        |> DE.andMap (D.oneOf [ tRecord, tList, tTuple, tLeaf, tUnion, tRef, tValue, tAny ])
        |> DE.andMap (D.field "title" D.string |> DE.withDefault "")
        |> DE.andMap (D.field "description" D.string |> DE.withDefault "")
        |> DE.andMap (D.field "examples" (D.list D.value) |> DE.withDefault [])
        |> DE.andMap (D.maybe (D.field "$anchor" D.string))


tRecord : Decoder Type
tRecord =
    D.succeed RecordFields
        |> DE.when (D.field "type" D.string) ((==) "object")
        |> DE.andMap (D.field "properties" (D.dict (D.lazy (\_ -> schDecoder))))
        |> DE.andMap (D.field "order" (D.list D.string))
        |> D.map TRecord


tList : Decoder Type
tList =
    D.succeed TList
        |> DE.when (D.field "type" D.string) ((==) "array")
        |> DE.andMap (D.field "items" (D.lazy (\_ -> schDecoder)))


tTuple : Decoder Type
tTuple =
    D.succeed TTuple
        |> DE.when (D.field "type" D.string) ((==) "array")
        |> DE.andMap (D.field "items" (D.list (D.lazy (\_ -> schDecoder))))


tLeaf : Decoder Type
tLeaf =
    D.succeed TLeaf
        |> DE.andMap (D.field "type" D.string |> D.andThen (whichLeaf >> D.succeed))


tUnion : Decoder Type
tUnion =
    D.succeed TUnion
        |> DE.andMap (D.field "anyOf" (D.list (D.lazy (\_ -> schDecoder))))


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


whichLeaf : String -> LeafType
whichLeaf t =
    case t of
        "string" ->
            TString

        "number" ->
            TNumber

        "integer" ->
            TInteger

        "boolean" ->
            TBool

        "null" ->
            TNull

        _ ->
            TNull


null : Sch
null =
    { type_ = TLeaf TNull
    , title = ""
    , description = ""
    , examples = [ E.null ]
    , anchor = Nothing
    }


any : Sch
any =
    { type_ = TAny
    , title = ""
    , description = ""
    , examples = []
    , anchor = Nothing
    }


listAny : Sch
listAny =
    { type_ = TList any
    , title = ""
    , description = ""
    , examples = []
    , anchor = Nothing
    }


emptyUnion : Sch
emptyUnion =
    { type_ = TUnion []
    , title = ""
    , description = ""
    , examples = []
    , anchor = Nothing
    }


init : D.Value -> ( Model, Cmd Msg )
init json =
    case D.decodeValue mainDecoder json of
        Ok model ->
            let
                currentFile =
                    model.currentFile

                newFile =
                    { currentFile | sch = get currentFile.id currentFile.sch }
            in
            ( { currentFile = newFile, anchorsModels = model.anchorsModels }, Cmd.none )

        Err err ->
            ( { currentFile = SchFile "" null, anchorsModels = [] }, Cmd.none )


type alias PatchPayload =
    { id : String
    , path : String
    , sch : Sch
    }


postschDecoder : Decoder PatchPayload
postschDecoder =
    D.succeed PatchPayload
        |> DE.andMap (D.field "id" D.string)
        |> DE.andMap (D.field "path" D.string)
        |> DE.andMap (D.field "sch" schDecoder)


replaceSch : Sch -> PatchPayload -> Sch
replaceSch schema { path, sch } =
    -- TODO : write access(path)
    sch


get : String -> Sch -> Sch
get path sch =
    let
        acc =
            { level = 0, parentHead = any, path = "", result = sch }

        toReduce ( sch_, acc_ ) =
            if "[" ++ path ++ "]" == acc_.path && acc_.level /= 0 then
                { acc_ | result = Tuple.first ( sch_, acc_.result ) }

            else
                acc_
    in
    .result (reduce_ toReduce acc sch)


modelRefs : Sch -> AnchorsModels
modelRefs sch =
    case sch.type_ of
        TRecord { fields, order } ->
            let
                maybeAnchorModel ( modelName, sch_ ) =
                    Maybe.map (\a -> ( a, modelName )) sch_.anchor
            in
            List.filterMap maybeAnchorModel (Dict.toList fields)

        _ ->
            []


reduce : (( Sch, b ) -> b) -> b -> Sch -> b
reduce fun b sch =
    let
        acc =
            { level = 0, parentHead = any, path = "", result = fun ( sch, b ) }

        toReduce ( sch_, acc_ ) =
            { acc_ | result = fun ( sch_, acc_.result ) }
    in
    .result (reduce_ toReduce acc sch)


type alias Acc b =
    { b | level : Int, parentHead : Sch, path : String }


reduce_ : (( Sch, Acc b ) -> Acc b) -> Acc b -> Sch -> Acc b
reduce_ fun acc sch =
    case sch.type_ of
        TRecord { fields, order } ->
            let
                nextAcc k acc_ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = any
                        , path = acc.path ++ "[" ++ k ++ "]"
                    }

                walk k acc_ =
                    Dict.get k fields
                        |> Maybe.withDefault null
                        |> (\sch_ -> reduce_ fun (fun ( sch_, nextAcc k acc_ )) sch_)
            in
            List.foldl walk acc order

        TList sch_ ->
            let
                nextAcc i acc_ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = listAny
                        , path = acc.path ++ "[]" ++ "[" ++ String.fromInt i ++ "]"
                    }

                walk ( i, sch__ ) acc_ =
                    reduce_ fun (fun ( sch_, nextAcc i acc_ )) sch__
            in
            List.foldl walk acc (List.map2 Tuple.pair (List.range 0 0) [ sch_ ])

        TTuple schs ->
            let
                nextAcc i acc_ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = listAny
                        , path = acc.path ++ "[]" ++ "[" ++ String.fromInt i ++ "]"
                    }

                walk ( i, sch_ ) acc_ =
                    reduce_ fun (fun ( sch_, nextAcc i acc_ )) sch_
            in
            List.foldl walk acc (List.map2 Tuple.pair (List.range 0 (List.length schs - 1)) schs)

        TUnion schs ->
            let
                nextAcc i acc_ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = listAny
                        , path = acc.path ++ "[]" ++ "[" ++ String.fromInt i ++ "]"
                    }

                walk ( i, sch_ ) acc_ =
                    reduce_ fun (fun ( sch_, nextAcc i acc_ )) sch_
            in
            List.foldl walk acc (List.map2 Tuple.pair (List.range 0 (List.length schs - 1)) schs)

        _ ->
            fun ( sch, acc )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        FileChange newModel ->
            let
                file =
                    newModel.currentFile

                file_ =
                    { file | sch = get file.id file.sch }
            in
            ( { model | currentFile = file_, anchorsModels = newModel.anchorsModels }, Cmd.none )

        PostSch postsch ->
            let
                file =
                    model.currentFile

                newSchema =
                    replaceSch file.sch postsch

                newfile =
                    { file | sch = newSchema }
            in
            ( { model | currentFile = newfile }, Cmd.none )



--- View layer ---


type Msg
    = Noop
    | PostSch PatchPayload
    | FileChange Model


type alias Config msg =
    { level : Int, tab : Int, toMsg : msg, parentHead : Sch, path : String, refs : AnchorsModels }


viewModule : Model -> Html Msg
viewModule model =
    let
        modelFile =
            model.currentFile

        initMeta =
            { level = 0, tab = 1, toMsg = Noop, parentHead = any, path = "", refs = model.anchorsModels }
    in
    div
        [ id modelFile.id
        , class "grid grid-cols-fit py-6 h-full gap-4 _model_number"
        , attribute "phx-capture-click" "select_sch"
        , attribute "phx-value-paths" modelFile.id
        , attribute "phx-hook" "moveable"
        , attribute "data-group" "body"
        , attribute "data-indent" "1.25rem"
        ]
        (viewItself (Fmodel modelFile.id modelFile.sch) initMeta)


viewModel : Fmodel -> Config msg -> Html msg
viewModel ({ sch } as fmodel) ui =
    case sch.type_ of
        TRecord recordFields ->
            lazy2 viewFolder fmodel ui

        TList sch_ ->
            lazy2 viewFolder fmodel ui

        TTuple schs ->
            lazy2 viewFolder fmodel ui

        TUnion schs ->
            lazy2 viewFolder fmodel ui

        TLeaf leaf ->
            lazy2 viewLeaf fmodel ui

        TRef ref ->
            lazy2 viewLeaf fmodel ui

        TValue json ->
            lazy2 viewLeaf fmodel ui

        TAny ->
            lazy2 viewLeaf fmodel ui


viewFolder : Fmodel -> Config msg -> Html msg
viewFolder fmodel ({ level, tab } as ui) =
    nav [ id ui.path, classList [ ( "sort-handle", True ), ( "bg-dark-gray rounded py-4 shadow", level == tab ) ] ]
        [ details [ attribute "open" "open" ]
            [ summary [ class "flex flex-col" ] [ viewFolderHeader fmodel ui ]
            , Keyed.node "article"
                [ id ("moveable__" ++ ui.path)
                , attribute "phx-hook" "moveable"
                , attribute "data-indent" (String.concat [ String.fromFloat (toFloat (level + 1) * 1.25), "rem" ])
                , classList [ ( "content-vis-auto", level == tab ) ]
                ]
                (List.map (\html -> ( ui.path, html )) (viewItself fmodel ui))
            ]
        ]


viewFolderHeader : Fmodel -> Config msg -> Html msg
viewFolderHeader fmodel ui =
    div [ class "relative dragover-hl flex flex-wrap items-start w-full" ]
        [ p [ class "absolute m-1 leading-4 text-gray-900 font-mono text-xs" ]
            [ span [ class "close-marker cursor-pointer select-none" ]
                [ text "+" ]
            , span
                [ class "open-marker cursor-pointer select-none" ]
                [ text "-" ]
            ]
        , viewKeyTypePair fmodel ui
        ]


viewItself : Fmodel -> Config msg -> List (Html msg)
viewItself ({ key, sch } as fmodel) ui =
    case sch.type_ of
        TRecord fields ->
            viewRecord viewModel fields ui

        TList sch_ ->
            if sch_.type_ == TAny then
                []

            else
                viewList viewModel [ sch_ ] ui

        TTuple schs ->
            viewList viewModel schs ui

        TUnion schs ->
            viewUnion viewModel schs ui

        _ ->
            []


viewRecord : (Fmodel -> Config msg -> a) -> RecordFields -> Config msg -> List a
viewRecord mapFn { fields, order } ui =
    let
        nextUI k =
            { ui
                | level = ui.level + 1
                , parentHead = any
                , path = ui.path ++ "[" ++ k ++ "]"
            }

        walk k =
            Dict.get k fields
                |> Maybe.withDefault null
                |> (\sch_ -> mapFn (Fmodel k sch_) (nextUI k))
    in
    List.map walk order


viewList : (Fmodel -> Config msg -> a) -> List Sch -> Config msg -> List a
viewList mapFn schs ui =
    let
        nextUI i =
            { ui | level = ui.level + 1, parentHead = listAny, path = ui.path ++ "[]" ++ "[" ++ i ++ "]" }

        walk i sch_ =
            let
                index =
                    String.fromInt i
            in
            mapFn (Fmodel index sch_) (nextUI index)
    in
    List.indexedMap walk schs


viewUnion : (Fmodel -> Config msg -> a) -> List Sch -> Config msg -> List a
viewUnion mapFn schs ui =
    let
        nextUI i =
            { ui | level = ui.level + 1, parentHead = emptyUnion, path = ui.path ++ "[]" ++ "[" ++ i ++ "]" }

        walk i sch_ =
            let
                index =
                    String.fromInt i
            in
            mapFn (Fmodel index sch_) (nextUI index)
    in
    List.indexedMap walk schs


viewLeaf : Fmodel -> Config msg -> Html msg
viewLeaf fmodel ui =
    nav [ id ui.path, classList [ ( "sort-handle", True ), ( "bg-dark-gray rounded py-4 shadow", ui.level == ui.tab ) ] ]
        [ viewKeyTypePair fmodel ui ]


viewKeyTypePair : Fmodel -> Config msg -> Html msg
viewKeyTypePair fmodel ui =
    div [ class "flex w-full leading-6" ]
        [ div
            [ class "indent"
            , style "padding-left" (String.concat [ String.fromFloat (toFloat ui.level * 1.25), "rem" ])
            , preventOnClick ui.toMsg
            ]
            []
        , viewKey fmodel ui
        , viewTypeOptions fmodel ui
        , div
            [ class "flex-1 px-1 text-right", preventOnClick ui.toMsg ]
            [ viewAddButton fmodel ui ]
        ]


viewAddButton : Fmodel -> Config msg -> Html msg
viewAddButton ({ sch } as fmodel) ui =
    case sch.type_ of
        TRecord _ ->
            span
                [ class "px-2 bg-indigo-500 rounded cursor-pointer"
                , attribute "phx-click" "add_field"
                , attribute "phx-value-field" "Record"
                , attribute "phx-value-path" ui.path
                ]
                [ text "+" ]

        TList _ ->
            span
                [ class "px-2 bg-indigo-500 rounded cursor-pointer"
                , attribute "phx-click" "add_field"
                , attribute "phx-value-field" "Record"
                , attribute "phx-value-path" ui.path
                ]
                [ text "+" ]

        TUnion _ ->
            span
                [ class "px-2 bg-indigo-500 rounded cursor-pointer"
                , attribute "phx-click" "add_field"
                , attribute "phx-value-field" "Record"
                , attribute "phx-value-path" ui.path
                ]
                [ text "+" ]

        _ ->
            text ""


viewKey : Fmodel -> Config msg -> Html msg
viewKey fmodel ui =
    div
        [ class "flex items-start text-sm"
        , preventOnClick ui.toMsg
        ]
        (view_key_ fmodel ui
            ++ [ template [] [ viewTextarea fmodel ui ]
               ]
        )


view_key_ : Fmodel -> Config msg -> List (Html msg)
view_key_ sch ({ level, tab } as ui) =
    if level == tab then
        viewTopKey sch ui

    else
        viewTreeKey sch ui


viewTopKey : Fmodel -> Config msg -> List (Html msg)
viewTopKey ({ sch } as fmodel) ui =
    case sch.type_ of
        TRecord _ ->
            [ span [ class "text-blue-500 mr-2" ] [ text "record" ]
            , viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text "=" ]
            ]

        TTuple _ ->
            [ span [ class "text-blue-500 mr-2" ] [ text "tuple" ]
            , viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text "=" ]
            ]

        TList _ ->
            [ span [ class "text-blue-500 mr-2" ] [ text "list" ]
            , viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text "=" ]
            ]

        TUnion _ ->
            [ span [ class "text-blue-500 mr-2" ] [ text "union" ]
            , viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text "=" ]
            ]

        _ ->
            [ span [ class "text-blue-500 mr-2" ] [ text "field" ]
            , viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text ":" ]
            ]


viewTreeKey : Fmodel -> Config msg -> List (Html msg)
viewTreeKey fmodel ui =
    case ui.parentHead.type_ of
        TLeaf _ ->
            [ viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text ":" ]
            ]

        TUnion _ ->
            [ viewKeyText { fmodel | key = "" } ui
            , span [ class "mx-2 text-base text-gray-600" ] [ text "|" ]
            ]

        _ ->
            [ viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text ":" ]
            ]


viewKeyText : Fmodel -> Config msg -> Html msg
viewKeyText sch ui =
    p
        [ class ""
        , style "_max-width"
            (if ui.level == 0 then
                "24rem"

             else
                "12rem"
            )
        , attribute "phx-value-path" ui.path
        ]
        [ viewKeyText_ sch ui ]


viewKeyText_ : Fmodel -> Config msg -> Html msg
viewKeyText_ { key } ui =
    case ui.parentHead.type_ of
        TTuple _ ->
            span [ class "break-words text-gray-600" ] [ text key ]

        TList _ ->
            span [ class "break-words text-gray-600" ] [ text "└" ]

        _ ->
            if ui.level == 0 then
                -- TODO : "<%= Utils.word_break_html(@key) %>"
                span [ class "break-words text-indigo-400" ] [ text key ]

            else
                -- TODO : "<%= Utils.word_break_html(@key) %>"
                span [ class "break-words text-gray-300 opacity-75" ] [ text key ]


viewTextarea : Fmodel -> Config msg -> Html msg
viewTextarea fmodel ui =
    div [] []


viewTypeOptions : Fmodel -> Config msg -> Html msg
viewTypeOptions fmodel ui =
    details [ class "relative min-w-0" ]
        [ summary [ class "block" ]
            [ div [ class "break-words rounded cursor-pointer select-none text-gray-500 text-xs" ]
                [ viewType fmodel ui ]
            ]
        , ul [ class "details-menu absolute mt-1 z-10 bg-gray-300 text-gray-800 border border-gray-900 rounded text-xs" ]
            [ li []
                [ input
                    [ type_ "text"
                    , autofocus True
                    , list "changeable_types"
                    , style "min-width" "30vw"
                    , attribute "phx-keyup" "change_type"
                    , attribute "phx-key" "Enter"
                    , attribute "phx-value-path" ui.path
                    ]
                    []
                ]
            ]
        ]


viewType : Fmodel -> Config msg -> Html msg
viewType fmodel ui =
    p [ class "text-blue-500 text-sm break-words", style "min-width" "2ch" ]
        [ viewType_ fmodel ui ]


viewType_ : Fmodel -> Config msg -> Html msg
viewType_ fmodel ui =
    if ui.level == ui.tab then
        viewTopType fmodel ui

    else
        viewTreeType fmodel ui


viewTopType : Fmodel -> Config msg -> Html msg
viewTopType { sch } ui =
    case sch.type_ of
        TRecord { fields } ->
            if Dict.isEmpty fields then
                span [ class "" ] [ text "{any}" ]

            else
                span [ class "" ] [ text "{ }" ]

        TList sch_ ->
            if sch_.type_ == TAny then
                span [ class "" ] [ text "[any]" ]

            else
                span [ class "cursor-pointer flex" ] [ typeText sch_ ui ]

        TTuple _ ->
            span [ class "self-center cursor-pointer select-none" ] [ text "( )" ]

        TLeaf _ ->
            span [ class "" ] [ typeText sch ui ]

        TUnion _ ->
            span [ class "self-center cursor-pointer select-none" ] [ text "||" ]

        _ ->
            span [ class "" ] [ typeText sch ui ]


viewTreeType : Fmodel -> Config msg -> Html msg
viewTreeType { sch } ui =
    case sch.type_ of
        TRecord { fields } ->
            if Dict.isEmpty fields then
                span [ class "self-center cursor-pointer text-sm select-none" ] [ text "{any}" ]

            else
                span [ class "self-center cursor-pointer text-sm select-none" ] [ text "{ }" ]

        TList sch_ ->
            if sch_.type_ == TAny then
                span [ class "" ] [ text "[any]" ]

            else
                span [ class "cursor-pointer flex" ] [ text "[", typeText sch_ ui, text "]" ]

        TTuple _ ->
            span [ class "self-center cursor-pointer text-sm select-none" ] [ text "( )" ]

        TLeaf _ ->
            span [ class "" ] [ typeText sch ui ]

        TUnion _ ->
            span [ class "self-center cursor-pointer select-none" ] [ text "||" ]

        TAny ->
            span [ class "self-center cursor-pointer select-none" ] [ text "any" ]

        _ ->
            span [ class "<%= error_class(assigns) %>" ] [ typeText sch ui ]



{- refNmae : List (ModelName, AnchorId) -> Sch -> String
   refNmae refPairs sch =
-}


typeText : Sch -> Config msg -> Html msg
typeText sch ui =
    let
        refName ref =
            Dict.fromList ui.refs
                |> Dict.get (String.dropLeft 1 ref)
                |> Maybe.withDefault "type#404"

        scalarDecoder =
            D.oneOf
                [ D.string
                , D.bool |> D.andThen (stringFromBool >> D.succeed)
                , D.int |> D.andThen (String.fromInt >> D.succeed)
                , D.float |> D.andThen (String.fromFloat >> D.succeed)
                , D.null "null"
                , D.succeed "value"
                ]

        valueTypeText json =
            case D.decodeValue scalarDecoder json of
                Ok "value" ->
                    text "value"

                Ok scalar ->
                    span [ class "text-green-700" ] [ text ("\"" ++ scalar ++ "\"") ]

                Err _ ->
                    text "type#500"
    in
    case sch.type_ of
        TRecord _ ->
            text "record"

        TList _ ->
            text "list"

        TTuple _ ->
            text "tuple"

        TUnion _ ->
            text "union"

        TLeaf TString ->
            text "string"

        TLeaf TNumber ->
            text "number"

        TLeaf TInteger ->
            text "integer"

        TLeaf TBool ->
            text "bool"

        TLeaf TNull ->
            text "null"

        TAny ->
            text "any"

        TRef ref ->
            span [ class "text-indigo-400" ] [ text (refName ref) ]

        TValue json ->
            valueTypeText json


stringFromBool : Bool -> String
stringFromBool v =
    case v of
        True ->
            "true"

        False ->
            "false"


template : List (Html.Attribute msg) -> List (Html msg) -> Html msg
template attributes children =
    node "template" attributes children


preventOnClick : msg -> Html.Attribute msg
preventOnClick toMsg =
    preventDefaultOn "click" (D.map (\msg -> ( msg, True )) (D.succeed toMsg))



--- Boundary ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        updateKind =
            D.oneOf
                [ D.map PostSch postschDecoder
                , D.map FileChange mainDecoder
                ]
    in
    stateUpdate (D.decodeValue updateKind >> Result.withDefault Noop)


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = viewModule
        , update = update
        , subscriptions = subscriptions
        }
