module Main exposing (Model, Msg(..), getPoses, init, main, subscriptions, update, view, viewPoses)

import Browser exposing (Document)
import Css exposing (display, inline)
import Html.Styled exposing (Html, a, button, div, em, h2, h3, h4, img, input, legend, li, table, td, text, toUnstyled, tr, ul)
import Html.Styled.Attributes as Attrs exposing (css, src, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map5, string)
import Random
import Random.List
import Time exposing (Posix, toMinute, toSecond, utc)



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
    , workoutPoses : List ( Maybe Pose, Maybe Posix )
    }


type GettingPoses
    = Failure Http.Error
    | Loading
    | Success (List Pose)


type FilteringPoses
    = Filtering
    | Finished (List Pose)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { original = Loading
      , poses = Filtering
      , filterNum = 0
      , filterBeginner = True
      , filterIntermediate = True
      , filterAdvanced = True
      , inWorkout = False
      , exerciseDuration = Time.millisToPosix 30000
      , breakDuration = Time.millisToPosix 5000
      , workoutPoses = []
      }
    , getPoses
    )



-- UPDATE


type Msg
    = LoadPoses
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            ( { model | breakDuration = Time.millisToPosix (t * 1000) }, Cmd.none )

        SetExerciseDuration t ->
            ( { model | exerciseDuration = Time.millisToPosix (t * 1000) }, Cmd.none )

        StartWorkout ->
            case model.poses of
                Finished p ->
                    let
                        sequence =
                            List.map (\i -> ( Just i, Just model.exerciseDuration )) p
                                |> List.intersperse
                                    ( Nothing, Just model.breakDuration )
                                |> (\l -> ( Nothing, Just model.breakDuration ) :: l)
                    in
                    ( { model | inWorkout = True, workoutPoses = sequence }, Cmd.none )

                Filtering ->
                    ( model, Cmd.none )

        CancelWorkout ->
            ( { model | inWorkout = False, workoutPoses = [] }, Cmd.none )

        Tick _ ->
            let
                poses =
                    case model.workoutPoses of
                        p :: ps ->
                            case p of
                                ( i, Just time ) ->
                                    let
                                        decremented =
                                            Time.millisToPosix (Time.posixToMillis time - 1000)
                                    in
                                    if decremented == Time.millisToPosix 0 then
                                        ps

                                    else
                                        ( i, Just decremented ) :: ps

                                e ->
                                    e :: ps

                        e ->
                            e
            in
            let
                newModel =
                    if List.length poses == 0 then
                        { model | inWorkout = False }

                    else
                        model
            in
            ( { newModel | workoutPoses = poses }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



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
                    [ Css.width <| Css.px 800
                    , Css.margin Css.auto
                    , Css.fontFamily Css.sansSerif
                    ]
                ]
                (h2 [ css [ Css.textAlign Css.center ] ] [ text "Roga" ]
                    :: (if model.inWorkout then
                            [ viewWorkout model ]

                        else
                            [ viewFilters model numPoses
                            , viewPoses model
                            ]
                       )
                )
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
        [ css
            [ Css.width <| Css.px 800
            , Css.margin Css.auto
            , Css.borderCollapse Css.collapse
            ]
        ]
        (tr []
            [ td
                [ Attrs.colspan 2 ]
                [ div
                    [ css
                        [ Css.displayFlex
                        , Css.alignItems Css.center
                        , Css.justifyContent Css.center
                        ]
                    ]
                    [ button
                        [ onClick CancelWorkout ]
                        [ text "Cancel Workout" ]
                    ]
                ]
            ]
            :: List.concatMap
                (\i ->
                    case i of
                        ( Just p, Just t ) ->
                            [ tr [ css [ Css.textAlign Css.center ] ]
                                [ td
                                    [ Attrs.colspan 2
                                    , css
                                        [ Css.borderBottom3 (Css.px 1) Css.dashed (Css.hex "#dddddd")
                                        , Css.borderTop3 (Css.px 1) Css.solid (Css.hex "#dddddd")
                                        , Css.padding (Css.px 20)
                                        , Css.fontSize (Css.px 20)
                                        ]
                                    ]
                                    [ div [] [ text "Exercise ", viewTime t ] ]
                                ]
                            , viewPose p
                            ]

                        ( Nothing, Just t ) ->
                            [ tr [ css [ Css.textAlign Css.center ] ]
                                [ td
                                    [ Attrs.colspan 2
                                    , css [ Css.padding (Css.px 20), Css.fontSize (Css.px 20) ]
                                    ]
                                    [ div [] [ text "Break ", viewTime t ] ]
                                ]
                            ]

                        _ ->
                            [ text "Error" ]
                )
                model.workoutPoses
        )


viewTime : Posix -> Html Msg
viewTime t =
    let
        min =
            toMinute utc t

        sec =
            toSecond utc t
    in
    text
        ((if min /= 0 then
            String.fromInt min ++ "m "

          else
            ""
         )
            ++ (String.fromInt sec ++ "s")
        )


viewFilters : Model -> Int -> Html Msg
viewFilters model numPoses =
    let
        breakDuration =
            Time.posixToMillis model.breakDuration // 1000

        exerciseDuration =
            Time.posixToMillis model.exerciseDuration // 1000
    in
    table [ css [ Css.width (Css.px 400), Css.margin Css.auto ] ]
        [ viewNumberInput "Number of Poses" 0 numPoses model.filterNum (\s -> FilterNum (String.toInt s |> Maybe.withDefault model.filterNum))
        , viewNumberInput "Break duration" 1 30 breakDuration (\s -> SetBreakDuration (String.toInt s |> Maybe.withDefault breakDuration))
        , viewNumberInput "Exercise duration" 10 60 exerciseDuration (\s -> SetExerciseDuration (String.toInt s |> Maybe.withDefault exerciseDuration))
        , viewCheckbox "Beginner" model.filterBeginner FilterBeginner
        , viewCheckbox "Intermediate" model.filterIntermediate FilterIntermediate
        , viewCheckbox "Advanced" model.filterAdvanced FilterAdvanced
        , tr []
            [ td [] [ button [ onClick Filter ] [ text "Filter" ] ]
            , td [] [ button [ onClick StartWorkout ] [ text "Start workout" ] ]
            ]
        ]


viewNumberInput : String -> Int -> Int -> Int -> (String -> Msg) -> Html Msg
viewNumberInput name min max val oi =
    tr []
        [ td [] [ legend [] [ text name ] ]
        , td []
            [ input
                [ type_ "number"
                , Attrs.min <| String.fromInt min
                , Attrs.max <| String.fromInt max
                , value <| String.fromInt val
                , onInput oi
                , css [ Css.width (Css.px 80) ]
                ]
                []
            ]
        ]


viewCheckbox : String -> Bool -> (Bool -> Msg) -> Html Msg
viewCheckbox l c oc =
    tr []
        [ td [] [ legend [ css [ display inline ] ] [ text l ] ]
        , td []
            [ input
                [ Attrs.type_ "checkbox"
                , Attrs.checked c
                , onCheck oc
                ]
                []
            ]
        ]


viewPoses : Model -> Html Msg
viewPoses model =
    case model.original of
        Success _ ->
            case model.poses of
                Finished poses ->
                    case List.length poses of
                        0 ->
                            div [ css [ Css.textAlign Css.center ] ] [ text "No poses to show" ]

                        _ ->
                            table [ css [ Css.width <| Css.px 800, Css.margin Css.auto, Css.borderCollapse Css.collapse ] ]
                                (List.map (\p -> viewPose p) poses)

                Filtering ->
                    text "Filtering..."

        Loading ->
            div [ css [ Css.width <| Css.px 800, Css.margin Css.auto ] ] [ text "Loading..." ]

        Failure _ ->
            div [ css [ Css.width <| Css.px 800, Css.margin Css.auto ] ] [ text "Failed to load" ]


viewPose : Pose -> Html Msg
viewPose p =
    tr
        [ css
            [ Css.hover [ Css.backgroundColor (Css.hex "#f5f5f5") ]
            ]
        ]
        [ td
            [ css
                [ Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#dddddd")
                , Css.borderTop3 (Css.px 1) Css.dashed (Css.hex "#ddd")
                ]
            ]
            [ div [ css [ Css.margin (Css.px 20) ] ]
                [ h3 [] [ text p.pose, em [] [ text (" (" ++ p.asana ++ ")") ] ]
                , h4 [] [ text p.level ]
                , ul []
                    (List.map (\b -> li [] [ text b ]) p.benefits)
                ]
            ]
        , td
            [ css
                [ Css.borderTop3 (Css.px 1) Css.dashed (Css.hex "#ddd")
                , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#ddd")
                ]
            ]
            [ a [ Attrs.href p.image ]
                [ img
                    [ src p.image, Attrs.width 128, css [ Css.margin (Css.px 20) ] ]
                    []
                ]
            ]
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
        { url = "roga/poses.json"
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
