module ScrabbleOracleUI exposing (..)

import Browser
import Html exposing (Html, button, pre, div, a, text, Attribute, Html, button, div, span, text, ul, li, p, input, hr, h2, h3, h4, strong)
import Html.Attributes exposing (target, href, style, class, tabindex, maxlength, disabled, type_)
import Html.Events exposing (on, preventDefaultOn, onClick, keyCode, onInput)
import Http exposing (..)
import List.Extra exposing (getAt, setAt)
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe
import Time


main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

-- URLS
oracleURL = "https://scrabble-oracle-api.herokuapp.com/tell-the-scrabble-oracle"

-- MODEL

type alias RowLabel = String
type alias Cells = List (String)
type Row = Row RowLabel (List (Int))

type Tile = Tile { letter: Maybe Char, color: String }
type Board = Board (List (List Tile))
type Direction = LeftToRight | Down
type Cursor = Cursor (Int, Int)
type Loading = Idle | Loading | Failure String | Success String
type alias BestPlay = { newBoard: String, word: String, score: Int }

type alias Model =
  { board : Board
  , rack : String
  , cursorPos : Cursor
  , direction : Direction
  , loading: Loading
  , maybeEndpoint : Maybe String
  , maybeBestPlay : Maybe BestPlay
  , stopPolling : Bool
  }

scheduleTask : Board -> String -> Cmd Msg
scheduleTask board rack =
  let
    data : Encode.Value
    data = Encode.object
        [ ( "board", Encode.string <| boardToString board)
        , ( "rack", Encode.string rack )
        ]
  in
    Http.post
      { url = oracleURL
      , body = data |> Http.jsonBody
      , expect = Http.expectString TaskScheduled
      }

bestPlayDecoder : Decode.Decoder BestPlay
bestPlayDecoder =
  Decode.map3 BestPlay
    (Decode.field "newBoard" Decode.string)
    (Decode.field "word" Decode.string)
    (Decode.field "score" Decode.int)

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

tileToChar : Tile -> Char
tileToChar (Tile { letter }) =
  case letter of
    Nothing -> '_'
    Just char -> Char.toUpper char

boardToString : Board -> String
boardToString (Board board) = List.concat board |> List.map tileToChar |> String.fromList

isBoardEmpty : Board -> Bool
isBoardEmpty (Board board) = List.concat board |> List.all (\(Tile { letter }) -> letter == Nothing)

initBoard : Board
initBoard = Board (List.map (List.map charToTile << String.toList) baseBoard)

init : () -> (Model, Cmd Msg)
init _ =
  ({ board = initBoard
  , rack = ""
  , cursorPos = Cursor (7, 7)
  , direction = LeftToRight
  , loading = Idle
  , maybeEndpoint = Nothing
  , maybeBestPlay = Nothing
  , stopPolling = False
  }, Cmd.none)

-- UPDATE
onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown letterConstructor =
  preventDefaultOn "keydown" (Decode.map alwaysPreventDefault (Decode.map letterConstructor keyCode))

alwaysPreventDefault : msg -> (msg, Bool)
alwaysPreventDefault msg = (msg, True)

type Msg = SetDirection Direction
  | SetCursor Cursor
  | SetLetter Int
  | SetRack String
  | ScheduleTask
  | TaskScheduled (Result Http.Error String)
  | BestPlayResponse (Result Http.Error BestPlay)
  | Tick Time.Posix

updateTile : Int -> Tile -> Tile
updateTile charCode (Tile ({ letter, color } as tile)) =
  case charCode of
    8 -> Tile { tile | letter = Nothing }
    32 -> Tile tile
    _ ->  Tile { tile | letter = Just (Char.fromCode charCode) }

updateCursor : Cursor -> Direction -> Bool -> Cursor
updateCursor (Cursor (x, y)) dir isBackSpace=
  case (dir, isBackSpace) of
    (LeftToRight, False) -> if y == 14
      then
        Cursor (modBy 15 (x + 1), 0)
      else
        Cursor (x, y + 1)
    (LeftToRight, True) -> if y == 0
      then
        Cursor (modBy 15 (x - 1), 14)
      else
        Cursor (x, y - 1)
    (Down, False) -> if x == 14
      then
        Cursor (0, modBy 15 (y + 1))
      else
        Cursor (x + 1, y)
    (Down, True) -> if x == 0
      then
        Cursor (14, modBy 15 (y - 1))
      else
        Cursor (x - 1, y)

updateBoard : Cursor -> Int -> Board -> Board
updateBoard (Cursor (x, y)) charCode (Board board) =
  let
    c = Char.fromCode charCode
  in
  if Char.isAlpha c || c == ' ' || charCode == 8 then
    case getAt x board of
      Nothing -> Board board
      Just rowX ->
        case getAt y rowX of
          Nothing -> Board board
          Just tile ->
            let
              newRowX = setAt y (updateTile charCode tile) rowX
              newBoard = setAt x newRowX board
            in
              Board newBoard
  else Board board

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ board, cursorPos, direction, loading, rack, maybeEndpoint, stopPolling } as model) =
  case msg of
    SetCursor cursor -> ({ model | cursorPos = cursor }, Cmd.none)
    SetLetter charCode ->
      let
          mergedCharCode =
            case charCode of
              9 -> 32 -- 9 is tab and 32 is space
              13 -> 32 -- 13 is enter
              _ -> charCode
          chr = Char.fromCode mergedCharCode
      in
        ({ model |
          board = updateBoard cursorPos mergedCharCode board,
          cursorPos =
            if Char.isAlpha chr || chr == ' ' || charCode == 8 -- backspace
            then updateCursor cursorPos direction (charCode == 8)
            else cursorPos
        }, Cmd.none)
    SetDirection dir -> ({ model | direction = dir }, Cmd.none)
    SetRack value -> ({ model | rack = String.toUpper value }, Cmd.none)
    ScheduleTask -> if String.length rack /= 7 then ({model | loading = Failure "RACK MUST HAVE 7 LETTERS"}, Cmd.none)
                    else if isBoardEmpty board then ({model | loading = Failure "BOARD MUST HAVE ≥ 1 LETTER." }, Cmd.none)
                    else ({ model | loading = Loading }, scheduleTask board rack)
    Tick time ->
      if stopPolling then (model, Cmd.none) else
      case (loading, maybeEndpoint) of
      (Success _, Just endpoint) -> (model, Http.get
        { url = endpoint
        , expect = Http.expectJson BestPlayResponse bestPlayDecoder
        })
      _ -> (model, Cmd.none)
    TaskScheduled result ->
      case result of
        Ok url ->
          ({model | loading = Success url, stopPolling = False, maybeBestPlay = Nothing, maybeEndpoint = Just url }, Cmd.none)
        Err err ->
          case err of
            BadUrl url ->
              ({model | loading = Failure <| "URL IS WRONG: " ++ url}, Cmd.none)
            Timeout ->
              ({model | loading = Failure "TIMEOUT!" }, Cmd.none)
            NetworkError ->
              ({model | loading = Failure "NETWORKING ERROR!" }, Cmd.none)
            BadStatus status  ->
              ({model | loading = Failure <| "UNEXPECTED STATUS: " ++ String.fromInt status }, Cmd.none)
            BadBody body ->
              ({model | loading = Failure <| "BAD BODY: " ++ body }, Cmd.none)
    BestPlayResponse result ->
      case result of
        Ok bestPlay -> ({model | stopPolling = True, maybeBestPlay = Just bestPlay}, Cmd.none)
        Err err -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 5000 Tick

-- VIEW

view : Model -> Html Msg
view ({board, cursorPos, direction, loading, maybeBestPlay} as model) =
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

      showLoadingDots : Html Msg
      showLoadingDots = span [ class "loading "] [ span [] [ "." |> text ]
                                                 , span [] [ "." |> text ]
                                                 , span [] [ "." |> text ]
                                                 ]

      showWord : Html Msg
      showWord = case maybeBestPlay of
        Nothing -> div [ ] [ "WORD: " |> text
                           , showLoadingDots
                           ]
        Just ({ word }) -> div [] [ span [] [ "WORD: " |> text ]
                                  , pre [ style "display" "inline-block"
                                        , style "margin" "0" ] [ word |> text ]
                                  ]
      showScore : Html Msg
      showScore = case maybeBestPlay of
        Nothing -> div [ ] [ "SCORE: " |> text
                           , showLoadingDots
                           ]
        Just ({ score }) -> div [] [ span [] [ "SCORE: " |> text ]
                                   , pre [ style "display" "inline-block"
                                         , style "margin" "0" ] [ String.fromInt score |> text ]
                                   ]
      showBoard : Html Msg
      showBoard = case maybeBestPlay of
        Nothing -> div [ ] [ "BOARD: " |> text
                           , showLoadingDots
                           ]
        Just ({ newBoard }) -> div [ style "display" "flex" ] [ span [] [ "BOARD: " |> text ]
                                   , pre [ style "display" "inline-block"
                                         , style "margin" "0" ] [ newBoard |> text ]
                                   ]
      showLoader : Html Msg
      showLoader =
        case loading of
          Idle -> div [] []
          Failure msg -> div [] [ "ERROR: " ++ msg |> text ]
          Loading -> div [] [ "WAITING FOR SERVER" |> text
                            , showLoadingDots
                            ]
          Success url -> div [] [ span [] [ "SERVER CONNECTED" |> text ]
                                , showWord
                                , showScore
                                , showBoard
                                ]
  in
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "font-family" "sans-serif"
        ] (
          div [ style "width" "100%"
              , style "max-width" "670px"
              ]
              [ div [ style "font-size" "calc(16pt + 3vw)"
                     ] [ "Scrabble Oracle" |> text ]
              , ul [ style "font-size" "16pt"
                   , style "padding-left" "0"
                   , style "list-style" "none"
                   ] [ li [] [ "Discover the highest scoring word!" |> text ]
                     , li [] [ "Recreate your board by clicking on any tile then start typing" |> text ]
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
                            ] [ button [ onClick ScheduleTask
                                       , style "background-color" "transparent"
                                       , style "outline" "none"
                                       , style "font-size" "16pt"
                                       ] [ "GET BEST WORD" |> text ]
                              ]
                              , hr [] []
                              , showLoader
                            ]
                        ]
                      )
