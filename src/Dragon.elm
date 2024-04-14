module Dragon exposing (..)

import Browser
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, placeholder, value, style)
import Svg exposing (svg, polyline)
import Svg.Attributes exposing (fill, stroke, strokeWidth, points, height, width, viewBox)

main =
  Browser.element
  { init = init
  , update = update
  , subscriptions = \_ -> Sub.none
  , view = view
  }

-- MODEL

type alias Model =
  { points : List Point
  , height : Int
  , width : Int
  , unit : Int
  , maxX : Int
  , minX : Int
  , maxY : Int
  , minY : Int
  , iteration : Int
  }

type alias Point =
  { x : Int
  , y : Int
  }

init : () -> (Model, Cmd Msg)
init _ =
    ( dragonEgg 15 (Model [] 0 0 0 0 0 0 0 0)
    , Cmd.none
    )

dragonEgg : Int -> Model -> Model
dragonEgg i model =
  let
      p = [ Point i i, Point 0 i ]
      h = i * 2
      w = i * 2
      u = i
      mxX = i
      mnX = 0
      mxY = i
      mnY = 0
  in
    { model | points = p
            , height = h
            , width = w
            , unit = u
            , maxX = mxX
            , minX = mnX
            , maxY = mxY
            , minY = mnY
            }

-- UPDATE

type Msg
  = Render
  | Iterate
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ( dragonEgg 15 (Model [] 0 0 0 0 0 0 0 0)
      , Cmd.none
      )
    Render ->
      (model, Cmd.none)
    Iterate ->
      let
          -- declare local variables for use in return
          points = model.points ++ (iteratePoints model.points)
          validatedPoints = validatePoints points (abs (getMinimumX points)) (abs (getMinimumY points))
          maxX = getMaximumX validatedPoints
          minX = getMinimumX validatedPoints
          maxY = getMaximumY validatedPoints
          minY = getMinimumY validatedPoints
          height = maxY + model.unit
          width = maxX + model.unit
      in
        ( (Model validatedPoints height width model.unit maxX minX maxY minY (model.iteration + 1))
        , Cmd.none
        )

abs : Int -> Int
abs i =
  if i < 0 then (i * -1) else i

validatePoints : List Point -> Int -> Int -> List Point
validatePoints points tX tY =
  List.map (\p -> transformPoint p tX tY) points

iteratePoints : List Point -> List Point
iteratePoints points =
  {-
    Method:
      - Rotate each point in list
      - Reverse list order
      - Transform first point in list to match end point in exisitng list
      - Transform all additional points using same transformation
  -}
  let
      reversed = List.reverse points -- reverse the existing points
      sectionInit =  -- get the section initial point to match
        case List.head reversed of
          Nothing ->
            Point 0 0
          Just i ->
            i
      sectionPoints = List.map rotateSvgPoint90cc reversed -- get section points
      firstSectionPoint =
        case List.head sectionPoints of
          Nothing ->
            Point 0 0
          Just i ->
            i
      -- get transforms
      transformX = sectionInit.x - firstSectionPoint.x
      transformY = sectionInit.y - firstSectionPoint.y
  in
    List.drop 1 (List.map (\p -> transformPoint p transformX transformY) sectionPoints)

transformPoint : Point -> Int -> Int -> Point
transformPoint point tX tY =
  Point (point.x + tX) (point.y + tY)

rotateSvgPoint90cc : Point -> Point
rotateSvgPoint90cc p =
  {-
    90deg cc rotation on standard axes
    (x, y) -> (-y, x)

    90deg cc rotation on svg axes
    (x, y) -> (y, -x)
  -}
  Point p.y (-1 * p.x)

getMaximumX : List Point -> Int
getMaximumX points =
  -- reverse the sort so that the highest value is at the front
  case List.head (List.reverse (List.sortBy .x points)) of
    Nothing ->
      0
    Just p ->
      p.x

getMinimumX : List Point -> Int
getMinimumX points =
  case List.head (List.sortBy .x points) of
    Nothing ->
      0
    Just p ->
      p.x

getMaximumY : List Point -> Int
getMaximumY points =
  -- reverse the sort so that the highest value is at the front
  case List.head (List.reverse (List.sortBy .y points)) of
    Nothing ->
      0
    Just p ->
      p.y

getMinimumY : List Point -> Int
getMinimumY points =
  case List.head (List.sortBy .y points) of
    Nothing ->
      0
    Just p ->
      p.y

-- VIEW

view : Model -> Html Msg
view model =
    div []
    [ div []
      [ button [ onClick Iterate, style "margin-right" "0.5rem" ] [ text "Iterate" ]
      , button [ onClick Reset, style "margin-right" "0.5rem" ] [ text "Reset" ]
      , text ("Iteration: " ++ (String.fromInt model.iteration))
      ]
    -- debug stuff
    -- , div [] [ text ("polyline points: " ++ (dragonToString model.points)) ]
    -- , div [] [ text ("max x: " ++ (String.fromInt model.maxX)) ]
    -- , div [] [ text ("min x: " ++ (String.fromInt model.minX)) ]
    -- , div [] [ text ("max y: " ++ (String.fromInt model.maxY)) ]
    -- , div [] [ text ("min y: " ++ (String.fromInt model.minY)) ]
    -- , div [] [ text ("viewbox height: " ++ (String.fromInt model.height)) ]
    -- , div [] [ text ("viewbox width: " ++ (String.fromInt model.width)) ]
    , svg [ width (String.fromInt model.width)
          , height (String.fromInt model.height)
          , viewBox ("0 0 " ++ (String.fromInt model.width) ++ " " ++ (String.fromInt model.height))
          , style "padding" "1rem"
          ] [ polyline  [ fill "none"
                        , stroke "black"
                        , strokeWidth "2"
                        , points (dragonToString model.points)
                        ] [] 
          ]
    ]

-- transform the points list to a string for use in the svg polyline

dragonToString : List Point -> String
dragonToString points =
  points |> List.map pointToString |> String.join " "

pointListToStringList : List Point -> List String
pointListToStringList points =
  List.map (\p -> (pointToString p) ++ " ") points

pointToString : Point -> String
pointToString point =
  (String.fromInt point.x) ++ "," ++ (String.fromInt point.y)
