module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (concat, concatMap, filter, indexedMap, length, map, range, repeat)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Set exposing (size)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Msg
    = Select Tile
    | DoNothing
    | BeginGame Model


type alias Tile =
    { status : Selection, key : Int, group : Int }


type alias Model =
    List Tile


type Selection
    = Selected
    | NotSelected
    | Matched


init : () -> ( Model, Cmd Msg )
init _ =
    ( []
    , shuffleBoard
        (indexedMap
            (\index group -> { status = NotSelected, key = index, group = group })
            (concat (repeat 2 (range 0 9)))
        )
    )


shuffleBoard model =
    generate BeginGame (shuffle model)


updateSelectionTo : Selection -> Tile -> Model -> Model
updateSelectionTo status selected model =
    map
        (\item ->
            if item.key == selected.key then
                { item | status = status }

            else
                item
        )
        model


isNewTurn : Model -> Bool
isNewTurn model =
    length (filter (\item -> item.status == Selected) model) >= 2


checkTurn : Model -> (Tile -> Tile)
checkTurn model =
    if size (Set.fromList (map .group (filter (\tile -> tile.status == Selected) model))) == 1 then
        \tile ->
            case tile.status of
                Selected ->
                    { tile | status = Matched }

                _ ->
                    tile

    else
        \tile ->
            case tile.status of
                Selected ->
                    { tile | status = NotSelected }

                _ ->
                    tile


beginTurn : Model -> Model
beginTurn model =
    map (checkTurn model) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select selection ->
            ( updateSelectionTo Selected
                selection
                (if isNewTurn model then
                    beginTurn model

                 else
                    model
                )
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )

        BeginGame initialModel ->
            ( initialModel, Cmd.none )


tileView : Tile -> Html Msg
tileView tile =
    button
        [ class "w-16 h-16 border border-color-pink-light rounded-full m-1 p-1 text-5xl"
        , case tile.status of
            Matched ->
                class "bg-green-lighter"

            _ ->
                class "hover:bg-pink-lighter"
        , onClick
            (case tile.status of
                NotSelected ->
                    Select tile

                Selected ->
                    Select tile

                _ ->
                    DoNothing
            )
        ]
        [ text
            (case tile.status of
                NotSelected ->
                    ""

                _ ->
                    String.fromInt tile.group
            )
        ]


view : Model -> Html Msg
view model =
    div [ class "container flex items-stretch flex-wrap max-w-xs mx-auto" ]
        (map tileView model)
