module Main exposing (Model, Msg(..), getPoses, init, main, subscriptions, update, view, viewPoses)

import Browser
import Css exposing (display, inline)
import Html.Styled exposing (Html, button, div, em, h2, h3, h4, img, input, legend, li, table, td, text, toUnstyled, tr, ul)
import Html.Styled.Attributes as Attrs exposing (css, src, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput)
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
        , view = view >> toUnstyled
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
    | Filter
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
    div [ css [ Css.fontFamily Css.sansSerif ] ]
        [ h2 [ css [ Css.textAlign Css.center ] ] [ text "Roga" ]
        , viewFilters model numPoses
        , viewPoses model
        ]


originalDefault : GettingPoses -> List Pose
originalDefault g =
    case g of
        Success p ->
            p

        _ ->
            []


viewFilters : Model -> String -> Html Msg
viewFilters model numPoses =
    table [ css [ Css.width <| Css.px 400, Css.margin Css.auto ] ]
        [ tr []
            [ td [] [ legend [] [ text "Number of Poses" ] ]
            , td []
                [ input
                    [ type_ "number"
                    , Attrs.min "0"
                    , Attrs.max numPoses
                    , value <| String.fromInt model.filterNum
                    , onInput
                        (\s -> FilterNum (String.toInt s |> Maybe.withDefault model.filterNum))
                    , css [ Css.width (Css.px 80) ]
                    ]
                    []
                ]
            ]
        , viewCheckbox "Beginner" model.filterBeginner FilterBeginner
        , viewCheckbox "Intermediate" model.filterIntermediate FilterIntermediate
        , viewCheckbox "Advanced" model.filterAdvanced FilterAdvanced
        , button [ onClick Filter ] [ text "Filter" ]
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
    tr [ css [ Css.hover [ Css.backgroundColor (Css.hex "#f5f5f5") ] ] ]
        [ td [ css [ Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#dddddd"), Css.borderTop3 (Css.px 1) Css.solid (Css.hex "#ddd") ] ]
            [ div [ css [ Css.margin (Css.px 20) ] ]
                [ h3 [] [ text p.pose, em [] [ text (" (" ++ p.asana ++ ")") ] ]
                , h4 [] [ text p.level ]
                , ul []
                    (List.map (\b -> li [] [ text b ]) p.benefits)
                ]
            ]
        , td [ css [ Css.borderTop3 (Css.px 1) Css.solid (Css.hex "#ddd"), Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#ddd") ] ]
            [ img
                [ src p.image, Attrs.width 128, css [ Css.margin (Css.px 20) ] ]
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
