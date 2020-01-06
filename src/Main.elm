module Main exposing (Model, Msg(..), getPoses, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
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
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb255
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
import Html exposing (h2, li, ul)
import Html.Attributes as Attrs
import Http
import Json.Decode exposing (Decoder, field, list, map5, string)
import Random
import Random.List
import SmoothScroll exposing (defaultConfig, scrollToWithOptions)
import Task
import Time exposing (Posix, toHour, toMinute, toSecond, utc)
import Url
import Url.Builder as URLBuilder exposing (relative)
import Url.Parser exposing (Parser, int, parse, query)
import Url.Parser.Query as Query


type alias QueryParam a =
    { name : String
    , default : a
    , parser : String -> a -> Query.Parser a
    , builder : String -> a -> URLBuilder.QueryParameter
    }


timeParser : String -> Posix -> Query.Parser Posix
timeParser n d =
    Query.map seconds
        (Query.map
            (Maybe.withDefault (toSecond utc d))
            (Query.int n)
        )


timeBuilder : String -> Posix -> URLBuilder.QueryParameter
timeBuilder n v =
    URLBuilder.int n (toSecond utc v)


breakDurationParam : QueryParam Posix
breakDurationParam =
    QueryParam "breakDuration" (seconds 5) timeParser timeBuilder


exerciseDurationParam : QueryParam Posix
exerciseDurationParam =
    QueryParam "exerciseDuration" (seconds 30) timeParser timeBuilder


numPosesParser : Query.Parser Int
numPosesParser =
    Query.map (Maybe.withDefault defaultQuery.numPoses) (Query.int "numPoses")


boolParser : Bool -> String -> Query.Parser Bool
boolParser default s =
    Query.map (Maybe.withDefault default) (Query.enum s (Dict.fromList [ ( "true", True ), ( "false", False ) ]))


beginnerParser : Query.Parser Bool
beginnerParser =
    boolParser defaultQuery.beginner "beginner"


intermediateParser : Query.Parser Bool
intermediateParser =
    boolParser defaultQuery.intermediate "intermediate"


advancedParser : Query.Parser Bool
advancedParser =
    boolParser defaultQuery.advanced "advanced"


type alias Query =
    { breakDuration : Posix
    , exerciseDuration : Posix
    , numPoses : Int
    , beginner : Bool
    , intermediate : Bool
    , advanced : Bool
    }


type QueryMsg
    = BreakDuration Int
    | ExerciseDuration Int
    | NumPoses Int
    | Beginner Bool
    | Intermediate Bool
    | Advanced Bool


defaultQuery : Query
defaultQuery =
    { breakDuration = breakDurationParam.default
    , exerciseDuration = exerciseDurationParam.default
    , numPoses = 10
    , beginner = True
    , intermediate = True
    , advanced = True
    }


parser : QueryParam a -> Query.Parser a
parser qp =
    qp.parser qp.name qp.default


queryParser : Parser (Query -> a) a
queryParser =
    query
        (Query.map6
            Query
            (parser breakDurationParam)
            (parser exerciseDurationParam)
            numPosesParser
            beginnerParser
            intermediateParser
            advancedParser
        )


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


builder : QueryParam a -> a -> URLBuilder.QueryParameter
builder qp v =
    qp.builder qp.name v


queryBuilder : Query -> String
queryBuilder q =
    relative []
        [ builder breakDurationParam q.breakDuration
        , builder exerciseDurationParam q.exerciseDuration
        , URLBuilder.int "numPoses" q.numPoses
        , URLBuilder.string "beginner" (boolToString q.beginner)
        , URLBuilder.string "intermediate" (boolToString q.intermediate)
        , URLBuilder.string "advanced" (boolToString q.advanced)
        ]


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
    in
    ( newQuery
    , Nav.pushUrl key (queryBuilder newQuery)
    )



-- THEME


grey : Color
grey =
    rgb255 221 221 221


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


main : Program () Model Msg
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
    { original : GettingPoses
    , poses : FilteringPoses
    , inWorkout : Bool
    , workoutComplete : Bool
    , workoutPoses : List Exercise
    , workoutIndex : Int
    , key : Nav.Key
    , query : Query
    }


type GettingPoses
    = Failure Http.Error
    | Loading
    | Success (List Pose)


type FilteringPoses
    = Filtering
    | Finished (List Pose)


seconds : Int -> Posix
seconds s =
    Time.millisToPosix (s * 1000)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        query =
            case
                parse queryParser url
            of
                Just q ->
                    q

                Nothing ->
                    defaultQuery
    in
    ( { original = Loading
      , poses = Filtering
      , inWorkout = False
      , workoutComplete = False
      , workoutPoses = []
      , workoutIndex = 0
      , key = key
      , query = query
      }
    , getPoses
    )



-- UPDATE


type Msg
    = NoOp
    | LoadPoses
    | GotPoses (Result Http.Error (List Pose))
    | FilterNum Int
    | FilterBeginner Bool
    | FilterIntermediate Bool
    | FilterAdvanced Bool
    | Filter
    | Filtered (List Pose)
    | SetBreakDuration Int
    | SetExerciseDuration Int
    | StartWorkout
    | CancelWorkout
    | CompleteWorkout
    | Tick Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadPoses ->
            ( { model | original = Loading }, getPoses )

        GotPoses result ->
            case result of
                Ok poses ->
                    ( { model
                        | original = Success poses
                        , poses = Finished poses
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | original = Failure err }, Cmd.none )

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
            case model.original of
                Success p ->
                    ( { model | poses = Filtering }, filterPoses model p )

                _ ->
                    ( model, Cmd.none )

        Filtered poses ->
            ( { model | poses = Finished poses }, Cmd.none )

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
                Finished p ->
                    let
                        positions =
                            List.map (\i -> Position ( i, model.query.exerciseDuration )) p

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

                Filtering ->
                    ( model, Cmd.none )

        CancelWorkout ->
            ( { model
                | inWorkout = False
                , workoutComplete = False
                , workoutPoses = []
              }
            , Cmd.none
            )

        CompleteWorkout ->
            ( { model
                | inWorkout = False
                , workoutComplete = False
                , workoutPoses = []
              }
            , Cmd.none
            )

        Tick _ ->
            if not model.inWorkout then
                ( model, Cmd.none )

            else if model.workoutComplete then
                ( model, Cmd.none )

            else
                let
                    ( j, poses ) =
                        let
                            f ( i, exercise ) ( newI, es ) =
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
                                in
                                if i == model.workoutIndex then
                                    let
                                        decremented =
                                            Time.millisToPosix (Time.posixToMillis time - 1000)
                                    in
                                    if decremented == Time.millisToPosix 0 then
                                        ( newI + 1, set exercise decremented :: es )

                                    else
                                        ( newI, set exercise decremented :: es )

                                else
                                    ( newI, set exercise time :: es )
                        in
                        List.indexedMap Tuple.pair model.workoutPoses
                            |> List.foldr f ( model.workoutIndex, [] )
                in
                let
                    newModel =
                        if j == List.length poses then
                            { model | inWorkout = True, workoutComplete = True, workoutIndex = j }

                        else
                            { model | workoutIndex = j }

                    cmd =
                        if model.inWorkout && model.workoutIndex /= j then
                            scrollToExercise newModel.workoutIndex

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


scrollToExercise : Int -> Cmd Msg
scrollToExercise i =
    Task.attempt (always NoOp)
        (scrollToWithOptions
            { defaultConfig
                | speed = 100
                , offset = 150
            }
            ("exercise" ++ String.fromInt i)
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Roga"
    , body =
        [ column
            [ width shrink
            , centerX
            ]
            (el [ centerX ] (html (h2 [] [ Html.text "Roga" ]))
                :: (if model.inWorkout then
                        [ viewWorkout model ]

                    else
                        [ viewFilters model, viewPoses model ]
                   )
            )
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
                    (viewButton CancelWorkout "Cancel Workout")
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
            [ width fill
            ]
            [ el [ width fill ]
                (column
                    [ spacing 10
                    , centerX
                    ]
                    [ viewNumberInput ("Number of Poses: " ++ String.fromInt model.query.numPoses) 1 50 model.query.numPoses FilterNum
                    , viewNumberInput ("Break duration: " ++ viewTime model.query.breakDuration) 0 30 breakDuration SetBreakDuration
                    , viewNumberInput ("Exercise duration: " ++ viewTime model.query.exerciseDuration) 10 60 exerciseDuration SetExerciseDuration
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
            , el [ width fill ] (el [ centerX ] (text <| viewTime workoutDuration))
            , el [ width fill ] (el [ centerX ] (viewButton StartWorkout "Start Workout"))
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


viewPoses : Model -> Element Msg
viewPoses model =
    case model.original of
        Success _ ->
            case model.poses of
                Finished poses ->
                    case poses of
                        [] ->
                            el [ Font.center ] (text "No poses to show")

                        _ ->
                            column [ width fill ]
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
                                    poses
                                )

                Filtering ->
                    el [ Font.center ] (text "Filtering...")

        Loading ->
            el [ Font.center ] (text "Loading...")

        Failure _ ->
            el [ Font.center ] (text "Failed to load")


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
        , html
            (ul []
                (List.map (\b -> li [] [ Html.text b ]) p.benefits)
            )
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


poseDecoder : Decoder Pose
poseDecoder =
    map5 Pose
        benefitsDecoder
        posenameDecoder
        asanaDecoder
        levelDecoder
        imageDecoder


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


preFilter : Bool -> Bool -> Bool -> List Pose -> List Pose
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


randomPoses : List Pose -> Int -> Random.Generator (List Pose)
randomPoses poses n =
    Random.List.shuffle poses
        |> Random.map (List.take n)
