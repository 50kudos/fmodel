port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (attribute, autofocus, class, classList, id, list, style, type_)
import Html.Events exposing (onClick, preventDefaultOn)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import InfiniteList
import Json.Decode as D exposing (Decoder, Value, succeed)
import Json.Decode.Extra as DE
import Json.Encode as E
import Regex


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
    , height : Int
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
    , fmodels : List Fmodel
    }


type alias AnchorsModels =
    List ( String, String )


type alias Model =
    { currentFile : SchFile
    , anchorsModels : AnchorsModels
    , infiniteList : InfiniteList.Model
    }


mainDecoder : Decoder Model
mainDecoder =
    D.succeed Model
        |> DE.andMap (D.field "currentFile" schFileDecoder)
        |> DE.andMap (D.field "anchorsModels" anchorsModelsDecoder)
        |> DE.andMap (D.succeed InfiniteList.init)


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
        |> DE.andMap (D.field "fmodels" schsDecoder)


schsDecoder : Decoder (List Fmodel)
schsDecoder =
    D.succeed Fmodel
        |> DE.andMap (D.index 0 D.string)
        |> DE.andMap (D.index 1 schDecoder)
        |> D.list


schDecoder : Decoder Sch
schDecoder =
    D.succeed Sch
        |> DE.andMap (D.oneOf [ tRecord, tList, tTuple, tLeaf, tUnion, tRef, tValue, tAny ])
        |> DE.andMap (D.field "title" D.string |> DE.withDefault "")
        |> DE.andMap (D.field "description" D.string |> DE.withDefault "")
        |> DE.andMap (D.field "examples" (D.list D.value) |> DE.withDefault [])
        |> DE.andMap (D.field "height" D.int |> DE.withDefault 0)
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


any : Sch
any =
    { type_ = TAny
    , title = ""
    , description = ""
    , examples = [ E.null ]
    , height = 0
    , anchor = Nothing
    }


null : Sch
null =
    { any | type_ = TLeaf TNull }


anyRecord : Sch
anyRecord =
    { any | type_ = TRecord (RecordFields Dict.empty []) }


listAny : Sch
listAny =
    { any | type_ = TList any }


emptyUnion : Sch
emptyUnion =
    { any | type_ = TUnion [] }


init : D.Value -> ( Model, Cmd Msg )
init json =
    case D.decodeValue mainDecoder json of
        Ok model ->
            ( { currentFile = model.currentFile
              , anchorsModels = model.anchorsModels
              , infiniteList = InfiniteList.init
              }
            , Cmd.none
            )

        Err err ->
            ( { currentFile = SchFile "" [], anchorsModels = [], infiniteList = InfiniteList.init }, Cmd.none )


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
replaceSch schema { id, path, sch } =
    let
        path_ =
            String.replace id "" path
    in
    getAndUpdate path_ (always sch) schema |> Tuple.second


get : String -> Sch -> Sch
get path sch =
    getAndUpdate path identity sch |> Tuple.first


getAndUpdate : String -> (Sch -> Sch) -> Sch -> ( Sch, Sch )
getAndUpdate path f sch =
    let
        acc =
            { level = 0, parentHead = anyRecord, path = "", result = Nothing, key = "", sch = sch }

        toReduce sch_ acc_ =
            let
                accessPath =
                    if acc_.level == 1 && not (String.startsWith "[" path) then
                        "[" ++ path ++ "]"

                    else
                        path

                mappedSch =
                    f acc_.sch
            in
            {- TODO: implement Halt | Cont instead of checking Nothing which will visit all nodes even if a sch is already found -}
            if accessPath == acc_.path && acc_.result == Nothing then
                { acc_ | result = Just mappedSch, sch = mappedSch }

            else
                acc_

        newAcc =
            mapReduce toReduce acc.sch acc

        newSch =
            newAcc.sch

        postSch =
            newAcc
                |> .result
                |> Maybe.withDefault any
    in
    ( postSch, newSch )


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


reduce : (Sch -> b -> b) -> b -> Sch -> b
reduce f b sch =
    let
        toReduce sch_ acc_ =
            { acc_ | result = f sch_ acc_.result }
    in
    { level = 0
    , parentHead = anyRecord
    , path = ""
    , key = ""
    , sch = sch
    , result = f sch b
    }
        |> mapReduce toReduce sch
        |> .result


type alias Acc b =
    { b | level : Int, parentHead : Sch, path : String, key : String, sch : Sch }


mapReduce : (Sch -> Acc b -> Acc b) -> Sch -> Acc b -> Acc b
mapReduce f sch acc =
    case acc.sch.type_ of
        TRecord { fields, order } ->
            let
                putAcc k acc_ sch_ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = anyRecord
                        , path = acc.path ++ "[" ++ k ++ "]"
                        , key = k
                        , sch = sch_
                    }

                reducer k sch_ ( schs, acc_ ) =
                    let
                        fAcc =
                            f sch_ (putAcc k acc_ sch_)

                        reducedAcc =
                            mapReduce f fAcc.sch fAcc

                        newSchs =
                            ( k, reducedAcc.sch ) :: schs
                    in
                    ( newSchs, reducedAcc )

                ( newFields, newAcc ) =
                    Dict.foldl reducer ( [], acc ) fields

                newRecord =
                    TRecord (RecordFields (Dict.fromList newFields) order)
            in
            f sch { newAcc | sch = { sch | type_ = newRecord } }

        TList sch_ ->
            let
                putAcc i acc_ sch__ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = listAny
                        , path = acc.path ++ "[]" ++ "[" ++ String.fromInt i ++ "]"
                        , key = String.fromInt i
                        , sch = sch__
                    }

                reducer i sch__ acc_ =
                    let
                        fAcc =
                            f sch_ (putAcc i acc_ sch__)

                        reducedAcc =
                            mapReduce f fAcc.sch fAcc
                    in
                    ( reducedAcc.sch, reducedAcc )

                ( newItem, newAcc ) =
                    reducer 0 sch_ acc

                newListItem =
                    TList newItem
            in
            f sch { newAcc | sch = { sch | type_ = newListItem } }

        TTuple schs ->
            let
                putAcc i acc_ sch__ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = listAny
                        , path = acc.path ++ "[]" ++ "[" ++ String.fromInt i ++ "]"
                        , key = String.fromInt i
                        , sch = sch__
                    }

                reducer i sch_ ( schs_, acc_ ) =
                    let
                        fAcc =
                            f sch_ (putAcc i acc_ sch_)

                        reducedAcc =
                            mapReduce f fAcc.sch fAcc

                        newSchs =
                            reducedAcc.sch :: schs_
                    in
                    ( newSchs, reducedAcc )

                ( newItems, newAcc ) =
                    schs
                        |> List.indexedMap Tuple.pair
                        |> List.foldr (\( k, v ) -> reducer k v) ( [], acc )

                newTuple =
                    TTuple newItems
            in
            f sch { newAcc | sch = { sch | type_ = newTuple } }

        TUnion schs ->
            let
                putAcc i acc_ sch__ =
                    { acc_
                        | level = acc.level + 1
                        , parentHead = listAny
                        , path = acc.path ++ "[]" ++ "[" ++ String.fromInt i ++ "]"
                        , key = String.fromInt i
                        , sch = sch__
                    }

                reducer i sch_ ( schs_, acc_ ) =
                    let
                        fAcc =
                            f sch_ (putAcc i acc_ sch_)

                        reducedAcc =
                            mapReduce f fAcc.sch fAcc

                        newSchs =
                            reducedAcc.sch :: schs_
                    in
                    ( newSchs, reducedAcc )

                ( newItems, newAcc ) =
                    schs
                        |> List.indexedMap Tuple.pair
                        |> List.foldr (\( k, v ) -> reducer k v) ( [], acc )

                newUnion =
                    TUnion newItems
            in
            f sch { newAcc | sch = { sch | type_ = newUnion } }

        _ ->
            f sch { acc | sch = sch }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        InfiniteListMsg infModel ->
            ( { model | infiniteList = infModel }, Cmd.none )

        FileChange newModel ->
            ( { model | currentFile = newModel.currentFile, anchorsModels = newModel.anchorsModels }, Cmd.none )

        PostSch postsch ->
            let
                newfile =
                    model.currentFile

                -- newSchema =
                --     replaceSch file.sch postsch
                -- newfile =
                --     { file | sch = newSchema }
            in
            ( { model | currentFile = newfile }, Cmd.none )



--- View layer ---


type Msg
    = Noop
    | PostSch PatchPayload
    | FileChange Model
    | InfiniteListMsg InfiniteList.Model


type alias Config msg =
    { fileId : String, level : Int, tab : Int, toMsg : msg, parentHead : Sch, path : String, refs : AnchorsModels }


initMeta model k =
    { fileId = model.currentFile.id
    , level = 1
    , tab = 1
    , toMsg = Noop
    , parentHead = anyRecord
    , path = "[" ++ k ++ "]"
    , refs = model.anchorsModels
    }


itemHeight : Int -> ( Config msg, Fmodel ) -> Int
itemHeight idx ( _, item ) =
    item.sch.height


containerHeight : Int
containerHeight =
    30000


infConfig : InfiniteList.Config ( Config msg, Fmodel ) msg
infConfig =
    InfiniteList.config
        { itemView = viewFmodel
        , itemHeight = InfiniteList.withVariableHeight itemHeight
        , containerHeight = containerHeight
        }


customContainer : SchFile -> List ( String, String ) -> List (Html Msg) -> Html Msg
customContainer modelFile styles children =
    ul
        [ id modelFile.id
        , class "grid grid-cols-fit py-6 h-full gap-4 _model_number"
        , attribute "phx-capture-click" "select_sch"
        , attribute "phx-value-paths" modelFile.id
        , attribute "data-group" "body"
        , attribute "data-indent" "1.25rem"
        , style "overflow" "scroll"
        , InfiniteList.onScroll InfiniteListMsg
        ]
        children


viewModule : Model -> Html Msg
viewModule model =
    let
        modelFile =
            model.currentFile

        items =
            List.map (\fmodel -> ( initMeta model fmodel.key, fmodel )) modelFile.fmodels
    in
    InfiniteList.view (infConfig |> InfiniteList.withCustomContainer (customContainer modelFile)) model.infiniteList items


viewFmodel : Int -> Int -> ( Config msg, Fmodel ) -> Html msg
viewFmodel idx listIdx ( ui, fmodel ) =
    lazy2 viewModel ui fmodel


viewModel : Config msg -> Fmodel -> Html msg
viewModel ui fmodel =
    case fmodel.sch.type_ of
        TRecord _ ->
            viewFolder fmodel ui

        TList _ ->
            viewFolder fmodel ui

        TTuple _ ->
            viewFolder fmodel ui

        TUnion _ ->
            viewFolder fmodel ui

        TLeaf _ ->
            viewLeaf fmodel ui

        TRef _ ->
            viewLeaf fmodel ui

        TValue _ ->
            viewLeaf fmodel ui

        TAny ->
            viewLeaf fmodel ui


viewFolder : Fmodel -> Config msg -> Html msg
viewFolder fmodel ({ level, tab } as ui) =
    -- let
    --     _ =
    --         Debug.log "path" ui.path
    -- in
    li [ id ui.path, classList [ ( "sort-handle", True ), ( "bg-dark-gray rounded py-4 shadow", level == 1 ) ] ]
        [ details [ attribute "open" "open" ]
            [ summary [ class "flex flex-col" ] [ viewFolderHeader fmodel ui ]
            , Keyed.ul
                [ attribute "data-indent" (String.concat [ String.fromFloat (toFloat level * 1.25), "rem" ])
                ]
                (viewItself ui fmodel)
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


viewItself : Config msg -> Fmodel -> List ( String, Html msg )
viewItself ui fmodel =
    -- let
    --     _ =
    --         Debug.log "path" ui.path
    -- in
    case fmodel.sch.type_ of
        TRecord r ->
            List.map (\k -> ( k, walkRecord r.fields ui k )) r.order

        TList sch_ ->
            if sch_.type_ == TAny then
                []

            else
                viewList [ sch_ ] ui

        TTuple schs ->
            viewList schs ui

        TUnion schs ->
            viewUnion schs ui

        _ ->
            []


toViewModel nextUI k sch_ =
    viewModel (nextUI k) (Fmodel k sch_)


nextRecordUI ui k =
    { ui
        | level = ui.level + 1
        , parentHead = anyRecord
        , path = ui.path ++ "[" ++ k ++ "]"
    }


walkRecord fields ui k =
    Dict.get k fields
        |> Maybe.withDefault any
        |> toViewModel (nextRecordUI ui) k


walkList : (String -> Config msg) -> Int -> Sch -> Html msg
walkList nextUI i sch_ =
    let
        index =
            String.fromInt i
    in
    viewModel (nextUI index) (Fmodel index sch_)


nextListUI ui i =
    { ui | level = ui.level + 1, parentHead = listAny, path = ui.path ++ "[]" ++ "[" ++ i ++ "]" }


viewList : List Sch -> Config msg -> List ( String, Html msg )
viewList schs ui =
    List.indexedMap (\i sch_ -> ( String.fromInt i, walkList (nextListUI ui) i sch_ )) schs


nextUnionUI ui i =
    { ui | level = ui.level + 1, parentHead = emptyUnion, path = ui.path ++ "[]" ++ "[" ++ i ++ "]" }


viewUnion : List Sch -> Config msg -> List ( String, Html msg )
viewUnion schs ui =
    List.indexedMap (\i sch_ -> ( String.fromInt i, walkList (nextUnionUI ui) i sch_ )) schs


viewLeaf : Fmodel -> Config msg -> Html msg
viewLeaf fmodel ui =
    li [ id ui.path, classList [ ( "sort-handle", True ), ( "bg-dark-gray rounded py-4 shadow", ui.level == 1 ) ] ]
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
                , attribute "phx-value-path" (ui.fileId ++ ui.path)
                ]
                [ text "+" ]

        TList _ ->
            span
                [ class "px-2 bg-indigo-500 rounded cursor-pointer"
                , attribute "phx-click" "add_field"
                , attribute "phx-value-field" "Record"
                , attribute "phx-value-path" (ui.fileId ++ ui.path)
                ]
                [ text "+" ]

        TUnion _ ->
            span
                [ class "px-2 bg-indigo-500 rounded cursor-pointer"
                , attribute "phx-click" "add_field"
                , attribute "phx-value-field" "Record"
                , attribute "phx-value-path" (ui.fileId ++ ui.path)
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
    if level == 1 then
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
        , style "_max-width: "
            (if ui.level == 1 then
                "24rem"

             else
                "12rem"
            )
        , attribute "phx-value-path" (ui.fileId ++ ui.path)
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
            if ui.level == 1 then
                span [ class "break-words text-indigo-400" ] (wordBreak key)

            else
                span [ class "break-words text-gray-300 opacity-75" ] (wordBreak key)


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
                    , attribute "phx-value-path" (ui.fileId ++ ui.path)
                    ]
                    []
                ]
            ]
        ]


viewType : Fmodel -> Config msg -> Html msg
viewType fmodel ui =
    p [ class "text-blue-500 text-sm break-words", style "min-width" "5ch" ]
        [ viewType_ fmodel ui ]


viewType_ : Fmodel -> Config msg -> Html msg
viewType_ fmodel ui =
    if ui.level == 1 then
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
            span [ class "text-indigo-400" ] (wordBreak (refName ref))

        TValue json ->
            valueTypeText json


stringFromBool : Bool -> String
stringFromBool v =
    case v of
        True ->
            "true"

        False ->
            "false"


wordBreak : String -> List (Html msg)
wordBreak string =
    let
        break =
            Regex.fromString "[A-Z][a-z]+|::|_|\\." |> Maybe.withDefault Regex.never

        matched =
            (Regex.find break string |> List.map .match) ++ [ "" ]

        remained =
            Regex.split break string
    in
    List.map2 (\m r -> [ m, r ]) matched remained
        |> List.concat
        |> List.filter ((/=) "")
        |> List.map text
        |> List.intersperse (wbr [] [])


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
