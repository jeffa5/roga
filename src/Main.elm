port module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element
    exposing
        ( Color
        , Element
        , alignLeft
        , alignRight
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , html
        , htmlAttribute
        , image
        , inFront
        , layout
        , link
        , maximum
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , shrink
        , spacing
        , spacingXY
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (audio)
import Html.Attributes as Attrs
import Http
import Json.Decode exposing (Decoder, field, list, map6, string)
import Random
import Random.List
import SmoothScroll exposing (defaultConfig, scrollToWithOptions)
import Task
import Time exposing (Posix, toHour, toMinute, toSecond, utc)
import Url
import Url.Builder as URLBuilder exposing (relative)
import Url.Parser exposing (Parser, int, parse, query)
import Url.Parser.Query as Query


port beep : () -> Cmd msg


port revbeep : () -> Cmd msg


toSeconds : Posix -> Int
toSeconds t =
    Time.posixToMillis t // 1000


timeParser : String -> Posix -> Query.Parser Posix
timeParser n d =
    Query.map seconds (intParser n (toSeconds d))


timeBuilder : String -> Posix -> URLBuilder.QueryParameter
timeBuilder n v =
    intBuilder n (toSeconds v)


intParser : String -> Int -> Query.Parser Int
intParser n d =
    Query.map (Maybe.withDefault d) (Query.int n)


intBuilder : String -> Int -> URLBuilder.QueryParameter
intBuilder n v =
    URLBuilder.int n v


boolDict : Dict String Bool
boolDict =
    Dict.fromList [ ( "true", True ), ( "false", False ) ]


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


boolParser : String -> Bool -> Query.Parser Bool
boolParser n d =
    Query.map (Maybe.withDefault d) (Query.enum n boolDict)


boolBuilder : String -> Bool -> URLBuilder.QueryParameter
boolBuilder n v =
    URLBuilder.string n (boolToString v)


intListParser : String -> List Int -> Query.Parser (List Int)
intListParser n _ =
    Query.map
        (\s ->
            Maybe.withDefault "" s
                |> String.split ","
                |> List.filterMap String.toInt
        )
        (Query.string n)


intListToString : List Int -> String
intListToString l =
    List.map String.fromInt l |> String.join ","


intListBuilder : String -> List Int -> URLBuilder.QueryParameter
intListBuilder n v =
    URLBuilder.string n (intListToString v)


type alias QueryParam a =
    { name : String
    , default : a
    , parser : String -> a -> Query.Parser a
    , builder : String -> a -> URLBuilder.QueryParameter
    }


params :
    { breakDuration : QueryParam Posix
    , exerciseDuration : QueryParam Posix
    , numPoses : QueryParam Int
    , beginner : QueryParam Bool
    , intermediate : QueryParam Bool
    , advanced : QueryParam Bool
    , poseIDs : QueryParam (List Int)
    }
params =
    { breakDuration = QueryParam "breakDuration" (seconds 10) timeParser timeBuilder
    , exerciseDuration = QueryParam "exerciseDuration" (seconds 60) timeParser timeBuilder
    , numPoses = QueryParam "numPoses" 10 intParser intBuilder
    , beginner = QueryParam "beginner" True boolParser boolBuilder
    , intermediate = QueryParam "intermediate" True boolParser boolBuilder
    , advanced = QueryParam "advanced" True boolParser boolBuilder
    , poseIDs = QueryParam "poseIDs" [] intListParser intListBuilder
    }


type alias Query =
    { breakDuration : Posix
    , exerciseDuration : Posix
    , numPoses : Int
    , beginner : Bool
    , intermediate : Bool
    , advanced : Bool
    , poseIDs : List Int
    }


defaultQuery : Query
defaultQuery =
    { breakDuration = params.breakDuration.default
    , exerciseDuration = params.exerciseDuration.default
    , numPoses = params.numPoses.default
    , beginner = params.beginner.default
    , intermediate = params.intermediate.default
    , advanced = params.advanced.default
    , poseIDs = params.poseIDs.default
    }



-- Parse query


parser : QueryParam a -> Query.Parser a
parser qp =
    qp.parser qp.name qp.default


queryParser : Parser (Query -> a) a
queryParser =
    query
        (Query.map7
            Query
            (parser params.breakDuration)
            (parser params.exerciseDuration)
            (parser params.numPoses)
            (parser params.beginner)
            (parser params.intermediate)
            (parser params.advanced)
            (parser params.poseIDs)
        )



-- Build query


paramBuilder : QueryParam a -> a -> Maybe URLBuilder.QueryParameter
paramBuilder qp v =
    if qp.default /= v then
        Just (qp.builder qp.name v)

    else
        Nothing


queryBuilder : Query -> String
queryBuilder q =
    relative []
        (List.filterMap (\i -> i)
            [ paramBuilder params.breakDuration q.breakDuration
            , paramBuilder params.exerciseDuration q.exerciseDuration
            , paramBuilder params.numPoses q.numPoses
            , paramBuilder params.beginner q.beginner
            , paramBuilder params.intermediate q.intermediate
            , paramBuilder params.advanced q.advanced
            , paramBuilder params.poseIDs q.poseIDs
            ]
        )



-- Update query


type QueryMsg
    = BreakDuration Int
    | ExerciseDuration Int
    | NumPoses Int
    | Beginner Bool
    | Intermediate Bool
    | Advanced Bool
    | SelectedPoses (List Int)
    | ResetFilters


updateQuery : QueryMsg -> Nav.Key -> Query -> ( Query, Cmd Msg )
updateQuery msg key query =
    let
        newQuery =
            case msg of
                BreakDuration t ->
                    { query | breakDuration = seconds t }

                ExerciseDuration t ->
                    { query | exerciseDuration = seconds t }

                NumPoses n ->
                    { query | numPoses = n }

                Beginner b ->
                    { query | beginner = b }

                Intermediate b ->
                    { query | intermediate = b }

                Advanced b ->
                    { query | advanced = b }

                ResetFilters ->
                    { defaultQuery | poseIDs = query.poseIDs }

                SelectedPoses l ->
                    { query | poseIDs = l }
    in
    ( newQuery
    , Nav.pushUrl key (queryBuilder newQuery)
    )



-- THEME


grey : Color
grey =
    rgb255 221 221 221


blue : Color
blue =
    rgb255 50 50 200


highlight : Color
highlight =
    rgb255 245 245 245


type alias Sides =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


sides : Sides
sides =
    { top = 0, right = 0, bottom = 0, left = 0 }



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Exercise
    = Position ( Pose, Posix )
    | Break Posix


type alias Model =
    { poses : GettingPoses
    , filtering : Bool
    , inWorkout : Bool
    , workoutComplete : Bool
    , workoutPoses : List Exercise
    , workoutIndex : Int
    , key : Nav.Key
    , query : Query
    , window : Window
    , device : Element.Device
    }


type GettingPoses
    = Failure Http.Error
    | Loading
    | Success (Dict Int Pose)


seconds : Int -> Posix
seconds s =
    Time.millisToPosix (s * 1000)


normalizedUrl : Url.Url -> Url.Url
normalizedUrl url =
    { url | path = "" }


type alias Window =
    { width : Int
    , height : Int
    }


windowDecoder : Decoder Window
windowDecoder =
    Json.Decode.map2 Window
        (field "width" Json.Decode.int)
        (field "height" Json.Decode.int)


init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init windowJson url key =
    let
        window =
            case
                Json.Decode.decodeValue windowDecoder windowJson
            of
                Ok w ->
                    w

                Err _ ->
                    { width = 0, height = 0 }

        query =
            case
                parse queryParser (normalizedUrl url)
            of
                Just q ->
                    q

                Nothing ->
                    defaultQuery
    in
    ( { poses = Loading
      , filtering = False
      , inWorkout = False
      , workoutComplete = False
      , workoutPoses = []
      , workoutIndex = 0
      , key = key
      , query = query
      , window = window
      , device = Element.classifyDevice window
      }
    , getPoses
    )



-- UPDATE


type Msg
    = NoOp
    | GotPoses (Result Http.Error (List Pose))
    | FilterNum Int
    | FilterBeginner Bool
    | FilterIntermediate Bool
    | FilterAdvanced Bool
    | Filter
    | Filtered (List Int)
    | Reset
    | SetBreakDuration Int
    | SetExerciseDuration Int
    | StartWorkout
    | CancelWorkout
    | CompleteWorkout
    | Tick Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Resize ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotPoses result ->
            case result of
                Ok poses ->
                    let
                        posesDict =
                            List.map (\p -> ( p.id, p )) poses
                                |> Dict.fromList

                        newModel =
                            { model | poses = Success posesDict }
                    in
                    if List.length model.query.poseIDs == 0 then
                        ( { newModel | filtering = True }, filterPoses model poses )

                    else
                        ( newModel, Cmd.none )

                Err err ->
                    ( { model | poses = Failure err }, Cmd.none )

        FilterNum n ->
            let
                ( query, cmd ) =
                    updateQuery (NumPoses n) model.key model.query
            in
            ( { model | query = query }, cmd )

        FilterBeginner b ->
            let
                ( query, cmd ) =
                    updateQuery (Beginner b) model.key model.query
            in
            ( { model | query = query }, cmd )

        FilterIntermediate b ->
            let
                ( query, cmd ) =
                    updateQuery (Intermediate b) model.key model.query
            in
            ( { model | query = query }, cmd )

        FilterAdvanced b ->
            let
                ( query, cmd ) =
                    updateQuery (Advanced b) model.key model.query
            in
            ( { model | query = query }, cmd )

        Filter ->
            case model.poses of
                Success p ->
                    ( { model | filtering = True }, filterPoses model (Dict.values p) )

                _ ->
                    ( model, Cmd.none )

        Filtered poseIDs ->
            let
                ( query, cmd ) =
                    updateQuery (SelectedPoses poseIDs) model.key model.query
            in
            ( { model | filtering = False, query = query }, cmd )

        Reset ->
            let
                ( query, cmd ) =
                    updateQuery ResetFilters model.key model.query
            in
            ( { model | query = query }, cmd )

        SetBreakDuration t ->
            let
                ( query, cmd ) =
                    updateQuery (BreakDuration t) model.key model.query
            in
            ( { model | query = query }, cmd )

        SetExerciseDuration t ->
            let
                ( query, cmd ) =
                    updateQuery (ExerciseDuration t) model.key model.query
            in
            ( { model | query = query }, cmd )

        StartWorkout ->
            case model.poses of
                Success poses ->
                    if not model.filtering && List.length model.query.poseIDs /= 0 then
                        let
                            positions =
                                List.map (\p -> Position ( p, model.query.exerciseDuration )) (getSelectedPoses poses model.query.poseIDs)

                            workout =
                                if Time.posixToMillis model.query.breakDuration /= 0 then
                                    List.intersperse (Break model.query.breakDuration) positions
                                        |> (\i -> Break model.query.breakDuration :: i)

                                else
                                    positions
                        in
                        ( { model
                            | inWorkout = True
                            , workoutComplete = False
                            , workoutIndex = 0
                            , workoutPoses = workout
                          }
                        , scrollToExercise 0
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelWorkout ->
            ( { model
                | inWorkout = False
                , workoutComplete = False
                , workoutPoses = []
              }
            , scrollToTitle
            )

        CompleteWorkout ->
            ( { model
                | inWorkout = False
                , workoutComplete = False
                , workoutPoses = []
              }
            , scrollToTitle
            )

        Tick _ ->
            if not model.inWorkout then
                ( model, Cmd.none )

            else if model.workoutComplete then
                ( model, Cmd.none )

            else
                let
                    ( j, poses, half ) =
                        let
                            f ( i, exercise ) ( newI, es, bp ) =
                                let
                                    time =
                                        case exercise of
                                            Position ( _, t ) ->
                                                t

                                            Break t ->
                                                t

                                    set e t =
                                        case e of
                                            Position ( p, _ ) ->
                                                Position ( p, t )

                                            Break _ ->
                                                Break t

                                    original_duration =
                                        case exercise of
                                            Position ( _, _ ) ->
                                                model.query.exerciseDuration

                                            Break _ ->
                                                model.query.breakDuration
                                in
                                if i == model.workoutIndex then
                                    let
                                        decremented =
                                            Time.millisToPosix (Time.posixToMillis time - 1000)
                                    in
                                    if decremented == Time.millisToPosix 0 then
                                        ( newI + 1, set exercise decremented :: es, False )

                                    else if Time.posixToMillis decremented == Time.posixToMillis original_duration // 2 then
                                        ( newI, set exercise decremented :: es, True )

                                    else
                                        ( newI, set exercise decremented :: es, False )

                                else
                                    ( newI, set exercise time :: es, bp )
                        in
                        List.indexedMap Tuple.pair model.workoutPoses
                            |> List.foldr f ( model.workoutIndex, [], False )
                in
                let
                    newModel =
                        if j == List.length poses then
                            { model | inWorkout = True, workoutComplete = True, workoutIndex = j }

                        else
                            { model | workoutIndex = j }

                    cmd =
                        if model.inWorkout && model.workoutIndex /= j then
                            Cmd.batch
                                [ scrollToExercise newModel.workoutIndex
                                , beep ()
                                ]

                        else if half then
                            revbeep ()

                        else
                            Cmd.none
                in
                ( { newModel | workoutPoses = poses }, cmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged _ ->
            ( model, Cmd.none )

        Resize ( w, h ) ->
            let
                window =
                    { width = w, height = h }
            in
            ( { model | window = window, device = Element.classifyDevice window }, Cmd.none )


scrollConfig : SmoothScroll.Config
scrollConfig =
    { defaultConfig | speed = 100, offset = 150 }


scrollToExercise : Int -> Cmd Msg
scrollToExercise i =
    Task.attempt (always NoOp)
        (scrollToWithOptions scrollConfig
            ("exercise" ++ String.fromInt i)
        )


scrollToTitle : Cmd Msg
scrollToTitle =
    Task.attempt (always NoOp) (scrollToWithOptions scrollConfig "title")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , onResize (\w h -> Resize ( w, h ))
        ]



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Roga"
    , body =
        [ column [ height fill, centerX ]
            [ el
                [ centerX
                , htmlAttribute (Attrs.id "title")
                , padding 20
                , Font.bold
                , Font.size 36
                ]
                (text "Roga")
            , column
                [ width (shrink |> maximum model.window.width)
                , height fill
                , centerX
                ]
                (if model.inWorkout then
                    [ html
                        (audio
                            [ Attrs.src "beep.wav"
                            , Attrs.id "beep"
                            , Attrs.type_ "audio/wav"
                            ]
                            []
                        )
                    , html
                        (audio
                            [ Attrs.src "revbeep.wav"
                            , Attrs.id "revbeep"
                            , Attrs.type_ "audio/wav"
                            ]
                            []
                        )
                    , viewWorkout model
                    ]

                 else
                    [ viewFilters model, viewPoses model ]
                )
            , row [ width fill, padding 10, Font.size 16 ]
                [ el [ width fill ]
                    (row [ centerX ]
                        [ el [] (text "Roga by ")
                        , link [ Font.color blue ]
                            { url = "https://jeffas.io"
                            , label = el [] (text "Jeffas")
                            }
                        ]
                    )
                , el [ width fill ]
                    (row [ centerX ]
                        [ el [] (text "Source available on ")
                        , link [ Font.color blue ]
                            { url = "https://github.com/jeffa5/roga"
                            , label = el [] (text "GitHub")
                            }
                        ]
                    )
                ]
            ]
            |> layout []
        ]
    }


viewExercise : Model -> ( Int, Exercise ) -> Element Msg
viewExercise model ( i, exercise ) =
    let
        isHighlighted =
            i == model.workoutIndex

        cancelButton =
            inFront
                (el
                    [ alignRight
                    , centerY
                    , paddingEach { sides | right = 10 }
                    ]
                    (viewButton CancelWorkout "Cancel")
                )

        time t =
            inFront
                (el
                    [ alignLeft
                    , centerY
                    , paddingEach { sides | left = 10 }
                    ]
                    (text
                        (if Time.posixToMillis t == 0 then
                            ""

                         else
                            viewTime t
                        )
                    )
                )
    in
    case exercise of
        Position ( pose, t ) ->
            column
                ([ width fill
                 , Border.widthXY 0 1
                 , Border.color grey
                 ]
                    ++ (if isHighlighted then
                            [ Background.color highlight ]

                        else
                            []
                       )
                )
                [ el
                    ([ width fill
                     , Font.center
                     , Border.widthEach { sides | bottom = 1 }
                     , Border.color grey
                     , Border.dashed
                     , padding 20
                     ]
                        ++ (if isHighlighted then
                                [ cancelButton, time t ]

                            else
                                []
                           )
                    )
                    (text "Exercise")
                , viewPose ("exercise" ++ String.fromInt i) pose
                ]

        Break t ->
            el
                (width fill
                    :: (if isHighlighted then
                            [ Background.color highlight
                            , cancelButton
                            , time t
                            ]

                        else
                            []
                       )
                )
                (viewBreak ("exercise" ++ String.fromInt i))


viewWorkout : Model -> Element Msg
viewWorkout model =
    column [ width fill ]
        ((List.indexedMap Tuple.pair model.workoutPoses
            |> List.map (viewExercise model)
         )
            ++ (if model.workoutIndex == List.length model.workoutPoses then
                    [ el
                        [ width fill
                        , Background.color highlight
                        ]
                        (el
                            [ centerX
                            , padding 10
                            , Attrs.id ("exercise" ++ String.fromInt model.workoutIndex) |> htmlAttribute
                            ]
                            (viewButton CompleteWorkout "Complete Workout")
                        )
                    ]

                else
                    []
               )
        )


viewBreak : String -> Element Msg
viewBreak id =
    el
        [ centerX
        , padding 20
        , htmlAttribute (Attrs.id id)
        ]
        (text <| "Break")


viewTime : Posix -> String
viewTime t =
    let
        hour =
            toHour utc t

        min =
            toMinute utc t

        sec =
            toSecond utc t
    in
    (if hour /= 0 then
        [ String.fromInt hour ++ "h" ]

     else
        []
    )
        ++ (if min /= 0 then
                [ String.fromInt min ++ "m" ]

            else
                []
           )
        ++ (if sec /= 0 then
                [ String.fromInt sec ++ "s" ]

            else if hour == 0 && min == 0 then
                [ String.fromInt sec ++ "s" ]

            else
                []
           )
        |> String.join " "


viewFilters : Model -> Element Msg
viewFilters model =
    let
        breakDuration =
            Time.posixToMillis model.query.breakDuration // 1000

        exerciseDuration =
            Time.posixToMillis model.query.exerciseDuration // 1000

        workoutDuration =
            model.query.numPoses
                * (Time.posixToMillis model.query.breakDuration + Time.posixToMillis model.query.exerciseDuration)
                |> Time.millisToPosix
    in
    column
        [ width fill ]
        [ wrappedRow
            [ width fill ]
            [ el [ width fill ]
                (column
                    [ spacing 10
                    , centerX
                    ]
                    [ viewNumberInput ("Number of Poses: " ++ String.fromInt model.query.numPoses) 1 50 model.query.numPoses FilterNum
                    , viewNumberInput ("Break duration: " ++ viewTime model.query.breakDuration) 0 30 breakDuration SetBreakDuration
                    , viewNumberInput ("Exercise duration: " ++ viewTime model.query.exerciseDuration) 10 120 exerciseDuration SetExerciseDuration
                    ]
                )
            , el [ width fill ]
                (column
                    [ spacing 20
                    , centerX
                    ]
                    [ viewCheckbox "Beginner" model.query.beginner FilterBeginner
                    , viewCheckbox "Intermediate" model.query.intermediate FilterIntermediate
                    , viewCheckbox "Advanced" model.query.advanced FilterAdvanced
                    ]
                )
            ]
        , wrappedRow [ width fill, paddingXY 0 10 ]
            [ el [ width fill ] (el [ centerX ] (viewButton Filter "Filter"))
            , el [ width fill ] (el [ centerX ] (viewButton Reset "Reset"))
            , el [ width fill ] (el [ centerX ] (text <| viewTime workoutDuration))
            , el [ width fill ] (el [ centerX ] (viewButton StartWorkout "Start"))
            ]
        ]


viewButton : Msg -> String -> Element Msg
viewButton op l =
    Input.button
        [ Border.rounded 5
        , Background.color grey
        , padding 10
        ]
        { onPress = Just op
        , label = text l
        }


viewNumberInput : String -> Int -> Int -> Int -> (Int -> Msg) -> Element Msg
viewNumberInput name min max val oi =
    Input.slider
        [ width (px 200)
        , behindContent
            (el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color grey
                , Border.rounded 5
                ]
                Element.none
            )
        ]
        { onChange = round >> oi
        , label = Input.labelAbove [] (text name)
        , min = toFloat min
        , max = toFloat max
        , value = toFloat val
        , thumb = Input.defaultThumb
        , step = Just 1
        }


viewCheckbox : String -> Bool -> (Bool -> Msg) -> Element Msg
viewCheckbox l c oc =
    Input.checkbox []
        { onChange = oc
        , icon = Input.defaultCheckbox
        , checked = c
        , label = Input.labelRight [] (text l)
        }


getSelectedPoses : Dict Int Pose -> List Int -> List Pose
getSelectedPoses poses ids =
    Dict.filter (\id _ -> List.any (\i -> i == id) ids) poses
        |> Dict.values


viewPoses : Model -> Element Msg
viewPoses model =
    case model.poses of
        Success poses ->
            if not model.filtering then
                case model.query.poseIDs of
                    [] ->
                        el [ width fill ] (el [ centerX ] (text "No poses to show"))

                    _ ->
                        column
                            [ width fill ]
                            (List.indexedMap
                                (\i pose ->
                                    el
                                        [ if i == 0 then
                                            Border.widthEach { sides | top = 1, bottom = 1 }

                                          else
                                            Border.widthEach { sides | bottom = 1 }
                                        , Border.color grey
                                        , width fill
                                        , height fill
                                        ]
                                        (viewPose "" pose)
                                )
                                (getSelectedPoses poses model.query.poseIDs)
                            )

            else
                el [ width fill ] (el [ centerX ] (text "Filtering..."))

        Loading ->
            el [ width fill ] (el [ centerX ] (text "Loading..."))

        Failure _ ->
            el [ width fill ] (el [ centerX ] (text "Failed to load"))


viewPoseText : Pose -> Element Msg
viewPoseText p =
    column
        [ height fill
        , width fill
        , spacingXY 0 10
        , paddingXY 0 20
        ]
        [ el []
            (paragraph
                [ Font.bold
                , Font.size 20
                ]
                [ text p.pose
                , el [ Font.italic ] (text (" (" ++ p.asana ++ ")"))
                ]
            )
        , el [ Font.bold, Font.size 18 ] (text p.level)
        , column [] (List.map (\b -> el [] (text ("• " ++ b))) p.benefits)
        ]


viewPoseImage : Pose -> Element Msg
viewPoseImage p =
    el
        [ height fill
        , paddingXY 0 20
        ]
        (link []
            { url = p.image
            , label =
                image [ width (px 128) ]
                    { src = p.image, description = p.pose }
            }
        )


viewPose : String -> Pose -> Element Msg
viewPose id pose =
    wrappedRow
        [ width fill
        , spacingXY 10 0
        , paddingXY 10 0
        , htmlAttribute (Attrs.id id)
        ]
        [ viewPoseImage pose
        , viewPoseText pose
        ]



-- HTTP


type alias Pose =
    { benefits : List String
    , pose : String
    , asana : String
    , level : String
    , image : String
    , id : Int
    }


benefitsDecoder : Decoder (List String)
benefitsDecoder =
    field "benefits" (Json.Decode.list string)


posenameDecoder : Decoder String
posenameDecoder =
    field "pose" string


asanaDecoder : Decoder String
asanaDecoder =
    field "asana" string


levelDecoder : Decoder String
levelDecoder =
    field "level" string


imageDecoder : Decoder String
imageDecoder =
    field "image" string


idDecoder : Decoder Int
idDecoder =
    field "id" Json.Decode.int


poseDecoder : Decoder Pose
poseDecoder =
    map6 Pose
        benefitsDecoder
        posenameDecoder
        asanaDecoder
        levelDecoder
        imageDecoder
        idDecoder


getPoses : Cmd Msg
getPoses =
    Http.get
        { url = "poses.json"
        , expect = Http.expectJson GotPoses posesDecoder
        }


posesDecoder : Decoder (List Pose)
posesDecoder =
    field "poses" (Json.Decode.list poseDecoder)



-- Random poses


filterPoses : Model -> List Pose -> Cmd Msg
filterPoses model poses =
    Random.generate Filtered
        (randomPoses
            (preFilter
                model.query.beginner
                model.query.intermediate
                model.query.advanced
                poses
            )
            model.query.numPoses
        )


preFilter : Bool -> Bool -> Bool -> List Pose -> List Int
preFilter b i a poses =
    poses
        |> List.filter
            (\p ->
                case p.level of
                    "Beginner" ->
                        b

                    "Intermediate" ->
                        i

                    "Advanced" ->
                        a

                    _ ->
                        True
            )
        |> List.map (\p -> p.id)


randomPoses : List Int -> Int -> Random.Generator (List Int)
randomPoses poseIDs n =
    Random.List.shuffle poseIDs
        |> Random.map (List.take n)
