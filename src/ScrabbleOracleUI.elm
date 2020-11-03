module ScrabbleOracleUI exposing (..)

import Browser
import Html exposing (Html, button, div, text, Attribute, Html, button, div, span, text, ul, li, p, input, hr, h2, h3, h4, strong)
import Html.Attributes exposing (style, class, tabindex, maxlength, type_)
import Html.Events exposing (on, preventDefaultOn, onClick, keyCode, onInput)
import List.Extra exposing (getAt, setAt)
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
  , rack : String
  , cursorPos : Cursor
  , direction : Direction
  , email : String
  , loading: Bool
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
    '_' -> Tile { letter = Nothing, color = "#cdc3a5" }
    _ -> Tile { letter = Just c, color = "#cdc3a5" }


initBoard : Board
initBoard = Board (List.map (List.map charToTile << String.toList) baseBoard)

init : Model
init =
  { board = initBoard
  , rack = ""
  , cursorPos = Cursor (7, 7)
  , direction = LeftToRight
  , email = ""
  , loading = False
  }

-- UPDATE
onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown letterConstructor =
  preventDefaultOn "keydown" (Json.map alwaysPreventDefault (Json.map letterConstructor keyCode))

alwaysPreventDefault : msg -> (msg, Bool)
alwaysPreventDefault msg = (msg, True)

type Msg = SetDirection Direction | SetCursor Cursor | SetLetter Int | SetRack String | SetEmail String

updateTile : Char -> Tile -> Tile
updateTile c (Tile ({ letter, color } as tile)) = Tile { tile | letter = (if c == ' ' then letter else Just c) }

updateCursor : Cursor -> Direction -> Cursor
updateCursor (Cursor (x, y)) dir =
  case dir of
    LeftToRight -> if y == 14
      then
        Cursor (remainderBy 15 (x + 1), 0)
      else
        Cursor (x, y + 1)
    Down -> if x == 14
      then
        Cursor (0, remainderBy 15 (y + 1))
      else
        Cursor (x + 1, y)

updateBoard : Cursor -> Char -> Board -> Board
updateBoard (Cursor (x, y)) c (Board board) =
  if Char.isAlpha c || c == ' ' then
    case getAt x board of
      Nothing -> Board board
      Just rowX ->
        case getAt y rowX of
          Nothing -> Board board
          Just tile ->
            let
              newRowX = setAt y (updateTile c tile) rowX
              newBoard = setAt x newRowX board
            in
              Board newBoard
  else Board board

update : Msg -> Model -> Model
update msg ({ board, cursorPos, direction } as model) =
  case msg of
    SetCursor cursor -> { model | cursorPos = cursor }
    SetLetter charCode ->
      let
          chr = Char.fromCode charCode
      in
        { model |
          board = updateBoard cursorPos chr board,
          cursorPos =
            if Char.isAlpha chr || chr == ' '
            then updateCursor cursorPos direction
            else cursorPos
        }
    SetDirection dir -> { model | direction = dir }
    SetRack value -> { model | rack = String.toUpper value }
    SetEmail value -> { model | email = value }

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
            , onKeyDown SetLetter
            , tabindex x
            , style "font-family" "'Work Sans', sans-serif"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "width" size
            , style "max-width" maxSize
            , style "height" size
            , style "max-height" maxSize
            , style "border" ("2px solid " ++ if hasCursor then "black" else "white")
            , style "border-radius" (if hasCursor then "3px" else "0px")
            , style "box-sizing" "border-box"
            , style "background-color" (if letter == Nothing then color else "rgb(255, 242, 203)")
            , style "font-size" "calc(14px + 1vw)"
            ] [ Maybe.withDefault ' ' letter |> String.fromChar |> text ]
  in
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "font-family" "sans-serif"
        ] (
          div [ style "width" "100%"
              , style "max-width" "670px"
              ]
              [ span [ style "font-size" "calc(16pt + 3vw)"
                     ] [ "Scrabble Oracle" |> text ]
              , span [ style "font-size" "16px"
                     , style "padding" "0 10px"
                     , style "white-space" "nowrap"
                     ] [ "find the highest scoring word" |> text ]
              , ul [ style "font-size" "16pt"
                   , style "padding-left" "0"
                   , style "list-style" "none"
                   ] [ li [ style "" "" ] [ "Recreate your board by clicking on any tile then start typing" |> text ]
                     , li [] [ hr [] [] ]
                     , li [ style "display" "flex"
                          , style "justify-content" "space-between"
                          , style "align-items" "center"
                          , style "flex-wrap" "wrap"
                          ] [ span [] [ "RACK: " |> text ]
                                    , input [ onInput SetRack
                                            , style "flex-grow" "1"
                                            , style "font-size" "16pt"
                                            , style "margin" "7px 0"
                                            , style "width" "13ch"
                                            , type_ "text"
                                            , maxlength 7
                                            ] []
                            , span [ style "margin" "0 10px" ] [ "DIRECTION:" |> text ]
                                    , button [ onClick (SetDirection (if direction == Down then LeftToRight else Down))
                                             , style "background-color" "transparent"
                                             , style "font-size" "16pt"
                                             , style "outline" "none"
                                             ] [ (if direction == Down then "DOWN" else "LTR") |> text ]
                           ]
                    ]
              ] :: List.indexedMap (\i ts -> div
                [ style "display" "flex"
                ] (List.indexedMap (showTile i) ts)) (getBoard board) ++ [
                  div [ style "width" "100%"
                      , style "max-width" "670px"
                      , style "font-size" "16pt"
                      ] [ div [ style "display" "flex"
                            , style "align-items" "center"
                            , style "margin-top" "10px"
                            ] [ span [ ] [ "EMAIL: " |> text ]
                              , input [ onInput SetEmail
                                      , style "margin" "7px 0"
                                      , style "font-size" "16pt"
                                      , style "width" "10ch"
                                      , style "flex-grow" "1"
                                      , type_ "text"
                                      ] []
                              , button [ style "background-color" "transparent"
                                       , style "outline" "none"
                                       , style "font-size" "16pt"
                                       , style "margin-left" "7px"
                                       ] [ "GET BEST WORD" |> text ]
                              ]
                            ]
                        ]
                      )
