module Main exposing (Model, Msg(..), getPoses, init, main, subscriptions, update, view)

import Browser exposing (Document)
import Css
import Element exposing (Element, el)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Styled exposing (Html, a, button, div, em, fromUnstyled, h2, h3, h4, img, li, table, td, text, toUnstyled, tr, ul)
import Html.Styled.Attributes as Attrs exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Http
import Interactive
import Json.Decode exposing (Decoder, field, list, map5, string)
import Random
import Random.List
import SmoothScroll exposing (defaultConfig, scrollToWithOptions)
import Task
import Time exposing (Posix, toMinute, toSecond, utc)



-- THEME


grey =
    Element.rgb255 200 200 200



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Exercise
    = Position ( Pose, Posix )
    | Break Posix


type alias Model =
    { original : GettingPoses
    , poses : FilteringPoses
    , filterNum : Int
    , filterBeginner : Bool
    , filterIntermediate : Bool
    , filterAdvanced : Bool
    , inWorkout : Bool
    , exerciseDuration : Posix
    , breakDuration : Posix
    , workoutPoses : List Exercise
    , workoutIndex : Int
    , interactive : Interactive.Model
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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( iModel, subCmd ) =
            Interactive.init
    in
    ( { original = Loading
      , poses = Filtering
      , filterNum = 0
      , filterBeginner = True
      , filterIntermediate = True
      , filterAdvanced = True
      , inWorkout = False
      , exerciseDuration = seconds 30
      , breakDuration = seconds 5
      , workoutPoses = []
      , workoutIndex = 0
      , interactive = iModel
      }
    , Cmd.batch
        [ getPoses
        , Cmd.map InteractiveMsg subCmd
        ]
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
    | Tick Posix
    | InteractiveMsg Interactive.Msg


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
                    ( { model | original = Success poses, poses = Finished poses, filterNum = List.length poses }, Cmd.none )

                Err err ->
                    ( { model | original = Failure err }, Cmd.none )

        FilterNum n ->
            ( { model | filterNum = n }, Cmd.none )

        FilterBeginner b ->
            ( { model | filterBeginner = b }, Cmd.none )

        FilterIntermediate b ->
            ( { model | filterIntermediate = b }, Cmd.none )

        FilterAdvanced b ->
            ( { model | filterAdvanced = b }, Cmd.none )

        Filter ->
            case model.original of
                Success p ->
                    ( { model | poses = Filtering }, filterPoses model p )

                _ ->
                    ( model, Cmd.none )

        Filtered poses ->
            ( { model | poses = Finished poses }, Cmd.none )

        SetBreakDuration t ->
            ( { model | breakDuration = seconds t }, Cmd.none )

        SetExerciseDuration t ->
            ( { model | exerciseDuration = seconds t }, Cmd.none )

        StartWorkout ->
            case model.poses of
                Finished p ->
                    let
                        sequence =
                            List.map (\i -> Position ( i, model.exerciseDuration )) p
                                |> List.intersperse
                                    (Break model.breakDuration)
                                |> (\l -> Break model.breakDuration :: l)
                    in
                    ( { model | inWorkout = True, workoutIndex = 0, workoutPoses = sequence }, scrollToExercise 0 )

                Filtering ->
                    ( model, Cmd.none )

        CancelWorkout ->
            ( { model | inWorkout = False, workoutPoses = [] }, Cmd.none )

        Tick _ ->
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
                        { model | inWorkout = False }

                    else
                        { model | workoutIndex = j }

                cmd =
                    if model.inWorkout then
                        scrollToExercise newModel.workoutIndex

                    else
                        Cmd.none
            in
            ( { newModel | workoutPoses = poses }, cmd )

        InteractiveMsg subMsg ->
            ( { model
                | interactive =
                    Interactive.update subMsg model.interactive
                        |> Tuple.first
              }
            , Cmd.none
            )


scrollToExercise : Int -> Cmd Msg
scrollToExercise i =
    Task.attempt (always NoOp) (scrollToWithOptions { defaultConfig | speed = 10, offset = 150 } ("exercise" ++ String.fromInt i))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Sub.map InteractiveMsg Interactive.subWindowResize
        ]



-- VIEW


view : Model -> Document Msg
view model =
    let
        numPoses =
            List.length (originalDefault model.original)
    in
    { title = "Roga"
    , body =
        [ toUnstyled <|
            div
                [ css
                    [ Css.maxWidth (Css.px 800)
                    , Css.margin Css.auto
                    , Css.fontFamily Css.sansSerif
                    ]
                ]
                [ Element.column
                    []
                    (el [ Element.centerX ] (Element.html (h2 [] [ text "Roga" ] |> toUnstyled))
                        :: (if model.inWorkout then
                                [ viewWorkout model |> toUnstyled |> Element.html ]

                            else
                                [ viewFilters model numPoses, viewPoses model ]
                           )
                    )
                    |> Element.layout []
                    |> fromUnstyled
                ]
        ]
    }


originalDefault : GettingPoses -> List Pose
originalDefault g =
    case g of
        Success p ->
            p

        _ ->
            []


viewWorkout : Model -> Html Msg
viewWorkout model =
    table
        [ css [ Css.width Css.inherit, Css.borderCollapse Css.collapse ] ]
        (tr []
            [ td
                [ Attrs.colspan 2 ]
                [ div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.justifyContent Css.center
                        , Css.paddingBottom (Css.em 1)
                        ]
                    ]
                    [ button
                        [ onClick CancelWorkout ]
                        [ text "Cancel Workout" ]
                    ]
                ]
            ]
            :: (let
                    f ( i, exercise ) =
                        let
                            highlight =
                                i == model.workoutIndex
                        in
                        let
                            time t =
                                if Time.posixToMillis t == 0 then
                                    "complete"

                                else
                                    viewTime t
                        in
                        case exercise of
                            Position ( pose, t ) ->
                                [ tr [ css [ Css.textAlign Css.center ], Attrs.id ("exercise" ++ String.fromInt i) ]
                                    [ td
                                        [ Attrs.colspan 2
                                        , css
                                            ([ Css.borderBottom3 (Css.px 1) Css.dashed (Css.hex "#dddddd")
                                             , Css.borderTop3 (Css.px 1) Css.solid (Css.hex "#dddddd")
                                             , Css.padding (Css.em 1)
                                             , Css.fontSize (Css.em 1)
                                             ]
                                                ++ (if highlight then
                                                        [ Css.backgroundColor (Css.hex "#f5f5f5") ]

                                                    else
                                                        []
                                                   )
                                            )
                                        ]
                                        [ div []
                                            [ text <|
                                                "Exercise "
                                                    ++ time t
                                            ]
                                        ]
                                    ]
                                , viewPose model pose highlight
                                ]

                            Break t ->
                                [ viewBreak ("exercise" ++ String.fromInt i) (time t) highlight ]
                in
                List.indexedMap Tuple.pair model.workoutPoses
                    |> List.concatMap f
               )
        )


viewBreak : String -> String -> Bool -> Html Msg
viewBreak id t highlight =
    tr [ css [ Css.textAlign Css.center ], Attrs.id id ]
        [ td
            [ Attrs.colspan 2
            , css
                ([ Css.padding (Css.em 1), Css.fontSize (Css.em 1) ]
                    ++ (if highlight then
                            [ Css.backgroundColor (Css.hex "#f5f5f5") ]

                        else
                            []
                       )
                )
            ]
            [ div []
                [ text <|
                    "Break "
                        ++ t
                ]
            ]
        ]


viewTime : Posix -> String
viewTime t =
    let
        min =
            toMinute utc t

        sec =
            toSecond utc t
    in
    if min /= 0 then
        if sec == 0 then
            String.fromInt min ++ "m"

        else
            String.fromInt min ++ "m " ++ String.fromInt sec ++ "s"

    else
        String.fromInt sec ++ "s"


viewFilters : Model -> Int -> Element Msg
viewFilters model numPoses =
    let
        breakDuration =
            Time.posixToMillis model.breakDuration // 1000

        exerciseDuration =
            Time.posixToMillis model.exerciseDuration // 1000

        ( width, height ) =
            model.interactive.windowSize
    in
    Element.column
        [ Element.width Element.fill ]
        [ Element.wrappedRow
            [ Element.width Element.fill
            ]
            [ el [ Element.width Element.fill ]
                (Element.column
                    [ Element.spacing 10
                    , Element.centerX
                    ]
                    [ viewNumberInput "Number of Poses" 0 numPoses model.filterNum FilterNum
                    , viewNumberInput ("Break duration: " ++ viewTime model.breakDuration) 1 30 breakDuration SetBreakDuration
                    , viewNumberInput ("Exercise duration: " ++ viewTime model.exerciseDuration) 10 60 exerciseDuration SetExerciseDuration
                    ]
                )
            , el [ Element.width Element.fill ]
                (Element.column
                    [ Element.spacing 20
                    , Element.centerX
                    ]
                    [ viewCheckbox "Beginner" model.filterBeginner FilterBeginner
                    , viewCheckbox "Intermediate" model.filterIntermediate FilterIntermediate
                    , viewCheckbox "Advanced" model.filterAdvanced FilterAdvanced
                    ]
                )
            ]
        , Element.wrappedRow [ Element.width Element.fill, Element.paddingXY 0 10 ]
            [ el [ Element.width Element.fill ] (el [ Element.centerX ] (viewButton Filter "Filter"))
            , el [ Element.width Element.fill ] (el [ Element.centerX ] (viewButton StartWorkout "Start Workout"))
            ]
        ]


viewButton : Msg -> String -> Element Msg
viewButton op l =
    Input.button
        [ Border.rounded 5
        , Background.color grey
        , Element.padding 10
        ]
        { onPress = Just op
        , label = Element.text l
        }


viewNumberInput : String -> Int -> Int -> Int -> (Int -> Msg) -> Element Msg
viewNumberInput name min max val oi =
    Input.slider
        [ Element.width (Element.px 200)
        , Element.behindContent
            (el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color grey
                , Border.rounded 5
                ]
                Element.none
            )
        ]
        { onChange = round >> oi
        , label = Input.labelAbove [] (Element.text name)
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
        , label = Input.labelRight [] (Element.text l)
        }


viewPoses : Model -> Element Msg
viewPoses model =
    case model.original of
        Success _ ->
            case model.poses of
                Finished poses ->
                    case List.length poses of
                        0 ->
                            el [ Font.center ] (Element.text "No poses to show")

                        _ ->
                            Element.table []
                                { data = poses
                                , columns =
                                    [ { header = Element.none
                                      , width = Element.fill
                                      , view = \pose -> viewPoseText pose False
                                      }
                                    , { header = Element.none
                                      , width = Element.fill
                                      , view = \pose -> viewPoseImage pose False
                                      }
                                    ]
                                }

                Filtering ->
                    el [ Font.center ] (Element.text "Filtering...")

        Loading ->
            el [ Font.center ] (Element.text "Loading...")

        Failure _ ->
            el [ Font.center ] (Element.text "Failed to load")


viewPoseText : Pose -> Bool -> Element Msg
viewPoseText p highlight =
    Element.column
        [ Border.widthXY 0 1
        , Border.color grey
        , Element.height Element.fill
        , Element.spacingXY 0 10
        , Element.paddingXY 0 20
        ]
        [ el []
            (Element.paragraph
                [ Font.bold
                , Font.size 20
                ]
                [ Element.text p.pose
                , el [ Font.italic ] (Element.text (" (" ++ p.asana ++ ")"))
                ]
            )
        , el [ Font.bold, Font.size 18 ] (Element.text p.level)
        , Element.html
            (ul []
                (List.map (\b -> li [] [ text b ]) p.benefits)
                |> toUnstyled
            )
        ]


viewPoseImage : Pose -> Bool -> Element Msg
viewPoseImage p highlight =
    el
        [ Border.widthXY 0 1
        , Border.color grey
        , Element.height Element.fill
        , Element.paddingXY 0 20
        ]
        (Element.link []
            { url = p.image
            , label =
                Element.image [ Element.width (Element.px 128) ]
                    { src = p.image, description = p.pose }
            }
        )


viewPose : Model -> Pose -> Bool -> Html Msg
viewPose model p highlight =
    let
        ( width, height ) =
            model.interactive.windowSize
    in
    tr
        [ css
            (if highlight then
                [ Css.backgroundColor (Css.hex "#f5f5f5") ]

             else
                []
            )
        ]
        (td
            [ css
                [ Css.width (Css.pct 80)
                , Css.marginLeft Css.auto
                , Css.marginRight Css.auto
                , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#dddddd")
                , Css.borderTop3 (Css.px 1) Css.dashed (Css.hex "#ddd")
                ]
            ]
            [ div [ css [ Css.margin (Css.em 1) ] ]
                [ h3 [] [ text p.pose, em [] [ text (" (" ++ p.asana ++ ")") ] ]
                , h4 [] [ text p.level ]
                , ul []
                    (List.map (\b -> li [] [ text b ]) p.benefits)
                ]
            ]
            :: (if width >= 800 then
                    [ td
                        [ css
                            [ Css.width (Css.pct 20)
                            , Css.borderTop3 (Css.px 1) Css.dashed (Css.hex "#ddd")
                            , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#ddd")
                            ]
                        ]
                        [ a [ Attrs.href p.image ]
                            [ img
                                [ src p.image, Attrs.width 128, css [ Css.margin (Css.em 1) ] ]
                                []
                            ]
                        ]
                    ]

                else
                    []
               )
        )



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
                model.filterBeginner
                model.filterIntermediate
                model.filterAdvanced
                poses
            )
            model.filterNum
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
