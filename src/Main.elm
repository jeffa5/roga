module Main exposing (Model, Msg(..), getPoses, init, main, subscriptions, update, view, viewPoses)

import Browser
import Element exposing (Element)
import Html exposing (Html, div, em, h2, h4, h5, img, input, legend, li, table, td, text, tr, ul)
import Html.Attributes as Attrs exposing (attribute)
import Html.Events exposing (onCheck, onInput)
import Http
import Json.Decode exposing (Decoder, field, list, map5, string)
import Random
import Random.Set
import Set



-- MAIN


main : Program () Model Msg
main =
    Browser.element
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
    | Filtered (List Pose)


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
            case model.original of
                Success p ->
                    let
                        newModel =
                            { model | filterNum = n, poses = Filtering }
                    in
                    ( newModel, filterPoses newModel p )

                _ ->
                    ( model, Cmd.none )

        FilterBeginner b ->
            case model.original of
                Success p ->
                    let
                        newModel =
                            { model | filterBeginner = b, poses = Filtering }
                    in
                    ( newModel, filterPoses newModel p )

                _ ->
                    ( model, Cmd.none )

        FilterIntermediate b ->
            case model.original of
                Success p ->
                    let
                        newModel =
                            { model | filterIntermediate = b, poses = Filtering }
                    in
                    ( newModel, filterPoses newModel p )

                _ ->
                    ( model, Cmd.none )

        FilterAdvanced b ->
            case model.original of
                Success p ->
                    let
                        newModel =
                            { model | filterAdvanced = b, poses = Filtering }
                    in
                    ( newModel, filterPoses newModel p )

                _ ->
                    ( model, Cmd.none )

        Filtered poses ->
            ( { model | poses = Finished poses }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        numPoses =
            List.length (originalDefault model.original) |> String.fromInt
    in
    div []
        [ h2 [] [ text "Roga" ]
        , div []
            [ legend [] [ text "Number of Poses" ]
            , input
                [ Attrs.type_ "number"
                , Attrs.min "0"
                , Attrs.max numPoses
                , Attrs.value <| String.fromInt model.filterNum
                , onInput
                    (\s -> FilterNum (String.toInt s |> Maybe.withDefault model.filterNum))
                ]
                []
            ]
        , viewCheckbox "Beginner" model.filterBeginner FilterBeginner
        , viewCheckbox "Intermediate" model.filterIntermediate FilterIntermediate
        , viewCheckbox "Advanced" model.filterAdvanced FilterAdvanced
        , viewPoses model
        ]


originalDefault : GettingPoses -> List Pose
originalDefault g =
    case g of
        Success p ->
            p

        _ ->
            []


viewCheckbox : String -> Bool -> (Bool -> Msg) -> Html Msg
viewCheckbox l c oc =
    div []
        [ legend [] [ text l ]
        , input
            [ Attrs.type_ "checked"
            , Attrs.checked c
            , onCheck oc
            ]
            []
        ]


viewPoses : Model -> Html Msg
viewPoses model =
    case model.original of
        Success _ ->
            case model.poses of
                Finished poses ->
                    table []
                        (List.map (\p -> viewPose p) poses)

                Filtering ->
                    text "Filtering..."

        Loading ->
            text "Loading..."

        Failure _ ->
            text "Failed to load"


viewPose : Pose -> Html Msg
viewPose p =
    tr []
        [ td []
            [ h4 [] [ text p.pose, em [] [ text (" (" ++ p.asana ++ ")") ] ]
            , h5 [] [ text p.level ]
            , ul []
                (List.map (\b -> li [] [ text b ]) p.benefits)
            ]
        , td []
            [ img
                [ attribute "src" p.image, Attrs.width 128 ]
                []
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
        { url = "../poses.json"
        , expect = Http.expectJson GotPoses posesDecoder
        }


posesDecoder : Decoder (List Pose)
posesDecoder =
    field "poses" (Json.Decode.list poseDecoder)



-- Random poses


filterPoses : Model -> List Pose -> Cmd Msg
filterPoses model poses =
    Random.generate Filtered (randomPoses (preFilter model.filterBeginner model.filterIntermediate model.filterAdvanced poses) model.filterNum)


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
    Random.Set.set
        (Basics.min n (List.length poses))
        (Random.int 0 (List.length poses))
        |> Random.map Set.toList
        |> Random.map (\l -> List.map (\i -> List.drop i poses |> List.head) l)
        |> Random.map
            (\l ->
                List.filterMap
                    (\x -> x)
                    l
            )
