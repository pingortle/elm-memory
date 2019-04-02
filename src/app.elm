module Main exposing (Method(..), main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, hidden, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import List exposing (concat, concatMap, filter, indexedMap, length, map, map2, range, repeat)
import Random
import Random.List exposing (shuffle)
import Set exposing (size)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


init : () -> ( Game, Cmd Method )
init _ =
    ( Game []
    , Cmd.batch
        [ Random.generate SetTiles
            (shuffle
                (indexedMap
                    (\index group -> { status = NotSelected, key = index, group = group, image = Nothing })
                    (concat (repeat 2 (range 0 9)))
                )
            )
        , Http.get
            { url = "https://api.giphy.com/v1/stickers/search?q=star+wars&limit=10&api_key=zzmTEoAr3EIiX7de4FMZGQdF3c8dHfW0"
            , expect = Http.expectJson GotGifs gifsDecoder
            }
        ]
    )


gifsDecoder =
    D.field "data" (D.list gifDecoder)


gifDecoder =
    D.map2 GiphyImage
        (D.field "images" (D.field "original" (D.field "url" D.string)))
        (D.field "images" (D.field "original_still" (D.field "url" D.string)))


update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        SetTiles tiles ->
            ( Game tiles, Cmd.none )

        GotGifs result ->
            case result of
                Ok data ->
                    ( setImages (Array.fromList data) model, Cmd.none )

                Err _ ->
                    ( setImages Array.empty model, Cmd.none )

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


setImages images model =
    { model | tiles = map (\tile -> { tile | image = Array.get tile.group images }) model.tiles }


view model =
    div [ class "grid mt-6" ]
        (map tileView model.tiles)


tileView tile =
    button
        [ class "border rounded-full m-1 text-5xl"
        , case tile.status of
            Matched ->
                class "border-green-light bg-green-lightest cursor-default"

            Selected ->
                class "hover:bg-pink-lightest border-pink-light"

            NotSelected ->
                class "hover:bg-pink-lightest"
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
        (case tile.image of
            Just image ->
                [ img
                    (concatMap themselves
                        [ [ class "w-full h-full rounded-full hover:opacity-75 object-cover" ]
                        , case tile.status of
                            Matched ->
                                [ class "opacity-75", src image.still ]

                            Selected ->
                                [ src image.animated ]

                            NotSelected ->
                                [ src image.animated, hidden True ]
                        ]
                    )
                    []
                ]

            Nothing ->
                []
        )


type Method
    = DoNothing
    | Guess Tile
    | SetTiles (List Tile)
    | GotGifs (Result Http.Error (List GiphyImage))


type alias Game =
    { tiles : List Tile }


type alias Tile =
    { status : Selection, key : Int, group : Int, image : Maybe GiphyImage }


type alias GiphyImage =
    { animated : String, still : String }


type Selection
    = Selected
    | NotSelected
    | Matched


type GameState
    = IncompleteGuess
    | IncorrectGuess
    | CorrectGuess
