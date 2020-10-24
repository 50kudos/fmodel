module Main exposing (..)

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


type alias Fmodel =
    { key : String
    , sch : Sch
    }


type alias Sch =
    { type_ : Type
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
    { models : List Fmodel
    }


schsDecoder : Decoder (List Fmodel)
schsDecoder =
    D.dict schDecoder
        |> D.map Dict.toList
        |> D.map (\fmodels -> List.map (\( key, sch ) -> Fmodel key sch) fmodels)


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
    { type_ = TLeaf TNull
    , title = ""
    , description = ""
    , examples = [ E.null ]
    }


any : Sch
any =
    { type_ = TAny
    , title = ""
    , description = ""
    , examples = []
    }


listAny : Sch
listAny =
    { type_ = TList any
    , title = ""
    , description = ""
    , examples = []
    }


emptyUnion : Sch
emptyUnion =
    { type_ = TUnion []
    , title = ""
    , description = ""
    , examples = []
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
                    [ Fmodel "null" null ]
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


type alias Config msg =
    { level : Int, tab : Int, toMsg : msg, parentHead : Sch, path : String }


viewModule : Model -> Html Msg
viewModule model =
    let
        tab =
            1

        toConfig modelNmae =
            { level = tab, tab = tab, toMsg = Noop, parentHead = any, path = "[" ++ modelNmae ++ "]" }
    in
    div
        [ id "model_root"
        , class "grid grid-cols-fit py-6 h-full gap-4 _model_number"
        , attribute "phx-capture-click" "select_sch"
        , attribute "phx-value-paths" "model_root"
        , attribute "phx-hook" "moveable"
        , attribute "data-group" "body"
        , attribute "data-indent" "1.25rem"
        , attribute "phx-update" "append"
        ]
        (List.map (\fmodel -> viewModel fmodel (toConfig fmodel.key)) model.models)


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
            viewRecord fields ui

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


viewRecord : RecordFields -> Config msg -> List (Html msg)
viewRecord { fields, order } ui =
    let
        nextUI k =
            { ui
                | level = ui.level + 1
                , parentHead = any
                , path = ui.path ++ "[" ++ k ++ "]"
            }

        viewNextModel k =
            Dict.get k fields
                |> Maybe.withDefault null
                |> (\sch_ -> viewModel (Fmodel k sch_) (nextUI k))
    in
    List.map viewNextModel order


viewList : List Sch -> Config msg -> List (Html msg)
viewList schs ui =
    let
        nextUI i =
            { ui | level = ui.level + 1, parentHead = listAny, path = ui.path ++ "[]" ++ "[" ++ i ++ "]" }

        viewNextModel i sch_ =
            let
                index =
                    String.fromInt i
            in
            viewModel (Fmodel index sch_) (nextUI index)
    in
    List.indexedMap viewNextModel schs


viewUnion : List Sch -> Config msg -> List (Html msg)
viewUnion schs ui =
    let
        nextUI i =
            { ui | level = ui.level + 1, parentHead = emptyUnion, path = ui.path ++ "[]" ++ "[" ++ i ++ "]" }

        viewNextModel i sch_ =
            let
                index =
                    String.fromInt i
            in
            viewModel (Fmodel index sch_) (nextUI index)
    in
    List.indexedMap viewNextModel schs


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
        TUnion _ ->
            [ viewKeyText fmodel ui
            , span [ class "mx-2 text-base text-gray-600" ] [ text "|" ]
            ]

        _ ->
            [ viewKeyText fmodel ui
            , span [ class "mx-2" ] [ text ":" ]
            ]


viewKeyText : Fmodel -> Config msg -> Html msg
viewKeyText sch ({ level, tab } as ui) =
    p
        [ class ""
        , style "_max-width"
            (if level == tab then
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
            if ui.level == ui.tab then
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
                span [ class "cursor-pointer flex" ] [ text "<%= read_type(Sch.items(@sch), @ui) %>" ]

        TTuple _ ->
            span [ class "self-center cursor-pointer select-none" ] [ text "( )" ]

        TLeaf _ ->
            span [ class "" ] [ text "read_type(@sch, @ui)" ]

        TUnion _ ->
            span [ class "self-center cursor-pointer select-none" ] [ text "||" ]

        _ ->
            span [ class "" ] [ text "<%= read_type(@sch, @ui) %>" ]


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
                span [ class "cursor-pointer flex" ] [ text "<%= read_type(Sch.items(@sch), @ui) %>" ]

        TTuple _ ->
            span [ class "self-center cursor-pointer text-sm select-none" ] [ text "( )" ]

        TLeaf _ ->
            span [ class "" ] [ text "read_type(@sch, @ui)" ]

        TUnion _ ->
            span [ class "self-center cursor-pointer select-none" ] [ text "||" ]

        TAny ->
            span [ class "self-center cursor-pointer select-none" ] [ text "any" ]

        _ ->
            span [ class "<%= error_class(assigns) %>" ] [ text "<%= read_type(@sch, @ui) %>" ]


template : List (Html.Attribute msg) -> List (Html msg) -> Html msg
template attributes children =
    node "template" attributes children


preventOnClick : msg -> Html.Attribute msg
preventOnClick toMsg =
    preventDefaultOn "click" (D.map (\msg -> ( msg, True )) (D.succeed toMsg))



--- Boundary ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = viewModule
        , update = update
        , subscriptions = subscriptions
        }
