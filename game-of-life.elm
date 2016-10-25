import Html exposing (Html, button, div, text)
import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Maybe exposing (andThen)
import Debug exposing (log)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- MODEL

size : Int
size = 20

type alias Cell = { x: Int, y: Int, value: Bool }

model : Array (Array Cell)
model =
  [0 .. (size - 1)]
  |> Array.fromList
  |> Array.map (\y ->
    [0 .. (size - 1)]
    |> Array.fromList
    |> Array.map (\x -> Cell x y False)
  )


-- UPDATE

type Msg
  = Invert Int Int
  | NewState


getCell : Int -> Int -> Array (Array Cell) -> Maybe Cell
getCell y x model = (Array.get y model) `andThen` (Array.get x)

setCell : Cell -> Array (Array Cell) -> Array (Array Cell)
setCell cell model = let { x, y, value } = cell in
  case Array.get y model of
    Just row ->
      case Array.get x row of
        Just c -> Array.set y (Array.set x cell row) model
        Nothing -> model
    Nothing -> model

invertCell : Int -> Int -> Array (Array Cell) -> Array (Array Cell)
invertCell y x model =
 let
   orig = getCell y x model
 in
 case log "original" orig of
  Just cell -> let { x, y, value } = cell
    in setCell (log "new" (Cell x y (not value))) model
  Nothing -> model

neighbourCoordinates : Int -> Int -> List (Int, Int)
neighbourCoordinates y x =
  List.filter (\c -> not ((fst c == x) && (snd c == y))) (List.concatMap (\a ->
    List.map (\b -> (a % size, b % size)) [ y - 1, y, y + 1 ]) [x - 1, x, x + 1])

transform : List (Maybe a) -> Maybe (List a)
transform ms =
  let
    f = (\x y -> case y of
      Just z -> x `andThen` (\x -> Just (x :: z))
      Nothing -> Nothing)
  in
  List.foldr f (Just []) ms

neighbours : Cell -> Array (Array Cell) -> List Cell
neighbours { x, y, value } model =
  let
    n = neighbourCoordinates y x
        |> List.map (\c -> getCell (snd c) (fst c) model)
        |> transform
  in
    case n of
      Just x -> x
      Nothing -> []

aliveCount : List Cell -> Int
aliveCount cells =
  cells
  |> List.filter (\x -> x.value)
  |> List.length

newCell : Cell -> Array (Array Cell) -> Cell
newCell cell model = let
    count = log "aliveCount" ((neighbours cell model) |> aliveCount)
    { x, y, value } = log "cell" cell
  in
    if value then
      if (count > 3) || (count < 2) then
        Cell x y False
      else
        Cell x y True
    else
      if count == 3 then
        Cell x y True
      else
        Cell x y False

newState : Array (Array Cell) -> Array (Array Cell)
newState model =
  log "newModel" (Array.map (\row -> Array.map (\cell -> newCell cell model) row) (log "origModel" model))

update : Msg -> Array (Array Cell) -> Array (Array Cell)
update msg model =
  case msg of
    Invert y x ->
      invertCell y x model
    NewState ->
      newState model

-- VIEW

cellView : Cell -> Html Msg
cellView { x, y, value } =
  div [ onClick (Invert y x), style
        [ ("display", "table-cell")
        , ("backgroundColor", if value then "red" else "gray")
        , ("height", "20px")
        , ("width", "20px")
        ]] [ ]

rowView : Array Cell -> Html Msg
rowView row =
  div [style [("display", "table-row")]] (List.map cellView (Array.toList row))

view :  Array (Array Cell) -> Html Msg
view model =
  div [] ((List.map rowView (Array.toList model)) ++ [ button [ onClick NewState ] [ text "new state" ]])
