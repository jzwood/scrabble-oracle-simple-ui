module ScrabbleOracleUI exposing (..)

import Browser
import Html exposing (Html, button, div, text, Attribute, Html, button, div, text, ul, li)
import Html.Attributes exposing (style, class, tabindex)
import Html.Events exposing (on, onClick, keyCode)
import List.Extra exposing (getAt)
import Json.Decode as Json
import List
import Maybe


main = Browser.sandbox
  { init = init
  , update = update
  , view = view
  }

-- MODEL

type alias RowLabel = String
type alias Cells = List (String)
type Row = Row RowLabel (List (Int))

type Tile = Tile { letter: Maybe Char, color: String }
type Board = Board (List (List Tile))
type Direction = LeftToRight | Down
type Cursor = Cursor (Int, Int)

type alias Model =
  { board : Board
  , cursorPos : Cursor
  , direction : Direction
  }

baseBoard : List String
baseBoard =
  [ "4__1___4___1__4"
  , "_2___3___3___2_"
  , "__2___1_1___2__"
  , "1__2___1___2__1"
  , "____2_____2____"
  , "_3___3___3___3_"
  , "__1___1_1___1__"
  , "4__1___2___1__4"
  , "__1___1_1___1__"
  , "_3___3___3___3_"
  , "____2_____2____"
  , "1__2___1___2___"
  , "__2___1_1___2__"
  , "_2___3___3___2_"
  , "4__1___4___1__4"
  ]

charToTile : Char -> Tile
charToTile c =
  case c of
    '1' -> Tile { letter = Nothing, color = "#bed9da" }
    '2' -> Tile { letter = Nothing, color = "#ffc6ba" }
    '3' -> Tile { letter = Nothing, color = "#0093ab" }
    '4' -> Tile { letter = Nothing, color = "#ff6d4d" }
    _ -> Tile { letter = Nothing, color = "#cdc3a5" }


initBoard : Board
initBoard = Board (List.map (List.map charToTile << String.toList) baseBoard)

init : Model
init =
  { board = initBoard
  , cursorPos = Cursor (7, 7)
  , direction = LeftToRight
  }

-- UPDATE

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)

type Msg = SetDirection Direction | SetCursor Cursor | SetChar Char

update : Msg -> Model -> Model
update msg ({ board, cursorPos, direction } as model) =
  case msg of
    SetCursor cursor -> { model | cursorPos = cursor }
    SetChar char -> { model | board = board}
    SetDirection dir -> { model | direction = dir }

-- VIEW

view : Model -> Html Msg
view ({board, cursorPos, direction} as model) =
  let
      size = "calc((100vw - 4px) / 15)"
      maxSize = "45px"
      getBoard (Board a) = a
      showTile : Int -> Int -> Tile -> Html Msg
      showTile x y (Tile { letter, color}) =
        let
            hasCursor = Cursor (x, y) == cursorPos
        in
          div
            [ onClick (SetCursor (Cursor (x, y)))
            , style "width" size
            , style "max-width" maxSize
            , style "height" size
            , style "max-height" maxSize
            , style "border" ("2px solid " ++ if hasCursor then "black" else "white")
            , style "border-radius" (if hasCursor then "4px" else "0px")
            , style "box-sizing" "border-box"
            , style "background-color" color
            ] [ Maybe.withDefault ' ' letter |> String.fromChar |> text ]
  in
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ] (
          div [ style "font-family" "'Work Sans', sans-serif"
              , style "width" "100%"
              , style "max-width" "667px"
              , style "padding-bottom" "10px"
              ]
              [
                div [ style "font-size" "calc(16pt + 3vw)"
                    ] [ "Scrabble Oracle" |> text ],
                div [ style "font-size" "16pt"
                    ] [ "Recreate your board by typing all tiles" |> text ]
              ] :: List.indexedMap (\i ts -> div
                [ style "display" "flex"
                ] (List.indexedMap (showTile i) ts)) (getBoard board))
