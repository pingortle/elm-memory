module Main exposing (Method(..), main, update, view)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List exposing (concat, concatMap, filter, indexedMap, length, map, range, repeat)
import Random
import Random.List exposing (shuffle)
import Set exposing (size)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Method )
init _ =
    ( Game []
    , Random.generate SetTiles
        (shuffle
            (indexedMap
                (\index group -> { status = NotSelected, key = index, group = group })
                (concat (repeat 2 (range 0 9)))
            )
        )
    )


update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        SetTiles tiles ->
            ( Game tiles, Cmd.none )

        Guess guess ->
            ( checkGuess guess model, Cmd.none )


checkGuess guess model =
    case stateForGuess guess model of
        IncompleteGuess ->
            { model | tiles = select guess model.tiles }

        IncorrectGuess ->
            { model | tiles = select guess (map (setStatusFor (is Selected .status) NotSelected) model.tiles) }

        CorrectGuess ->
            { model | tiles = map (setStatusFor (is Selected .status) Matched) (select guess model.tiles) }


select selection tiles =
    map (setStatusForKey Selected selection.key) tiles


stateForGuess guess model =
    if equalSets .key (guess :: are .status Selected model.tiles) (are .group guess.group model.tiles) then
        CorrectGuess

    else if length (are themselves Selected (map .status model.tiles)) < 2 then
        IncompleteGuess

    else
        IncorrectGuess


equalSets selector a b =
    Set.fromList (map selector a) == Set.fromList (map selector b)


are value selector list =
    filter (is selector value) list


is value selector =
    \comparable -> selector comparable == value


themselves value =
    value


setStatusForKey status key =
    \tile ->
        if key == tile.key then
            { tile | status = status }

        else
            tile


setStatusFor predicate status =
    \tile ->
        if predicate tile then
            { tile | status = status }

        else
            tile


view model =
    div [ class "container flex items-stretch flex-wrap max-w-xs mx-auto" ]
        (map tileView model.tiles)


tileView tile =
    button
        [ class "w-16 h-16 border border-color-pink-light rounded-full m-1 text-5xl"
        , case tile.status of
            Matched ->
                class "bg-green-lighter"

            _ ->
                class "hover:bg-pink-lighter"
        , onClick
            (case tile.status of
                NotSelected ->
                    Guess tile

                Selected ->
                    Guess tile

                Matched ->
                    DoNothing
            )
        ]
        (case tile.status of
            NotSelected ->
                []

            _ ->
                [ img [ class "w-full h-full", src "https://media.giphy.com/media/8nbGW1erXutpu/giphy.gif" ] []
                ]
        )


type Method
    = DoNothing
    | Guess Tile
    | SetTiles (List Tile)


type alias Game =
    { tiles : List Tile }


type alias Tile =
    { status : Selection, key : Int, group : Int }


type Selection
    = Selected
    | NotSelected
    | Matched


type GameState
    = IncompleteGuess
    | IncorrectGuess
    | CorrectGuess
