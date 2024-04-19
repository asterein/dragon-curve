module RainbowDragon exposing (..)

import Browser
import Colors exposing (colors, getColorByIndex)
import Random
import Array
import Task
import Html exposing (Html, div, text, button, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, placeholder, value, style)
import Svg exposing (svg, polyline, g, defs, linearGradient, stop)
import Svg.Attributes exposing  ( fill
                                , stroke
                                , strokeWidth
                                , points
                                , height
                                , width
                                , viewBox
                                , transform
                                , id
                                , x1
                                , x2
                                , y1
                                , y2
                                , offset
                                , stopColor
                                )

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
  , scales : List DragonScale
  , height : Int
  , width : Int
  , unit : Int
  , maxX : Int
  , minX : Int
  , maxY : Int
  , minY : Int
  , iteration : Int
  , color : Int
  }

type alias Point =
  { x : Int
  , y : Int
  }

type alias DragonScale =
  { points : List Point
  , color : Int
  }

init : () -> (Model, Cmd Msg)
init _ = (dragonEgg 15, Cmd.none)

dragonEgg : Int -> Model
dragonEgg i =
  let
      p = [ Point i i, Point 0 i ]
      h = i * 2
      w = i * 2
      u = i
      mxX = i
      mnX = 0
      mxY = i
      mnY = 0
      model = Model [] [] 0 0 0 0 0 0 0 0 0
  in
    { model | points = p
            , scales = [ DragonScale p 0 ]
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
  = Render (List Point) Int
  | Iterate
  | Revert
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ( dragonEgg 15
      , Cmd.none
      )
    Render points iteration ->
      let
            validatedPoints = validatePoints points (abs (getMinimumX points)) (abs (getMinimumY points))
            maxX = getMaximumX validatedPoints
            minX = getMinimumX validatedPoints
            maxY = getMaximumY validatedPoints
            minY = getMinimumY validatedPoints
            height = maxY + model.unit
            width = maxX + model.unit
      in
          ({ model | points = validatedPoints
                    , scales = convertPointsToScales validatedPoints []
                    , height = height
                    , width = width
                    , maxX = maxX
                    , minX = minX
                    , maxY = maxY
                    , minY = minY
                    , iteration = iteration
                    }
          , Cmd.none)
    Revert ->
      if model.iteration <= 1 then
        (dragonEgg model.unit, Cmd.none)
      else if model.iteration > 1 then
        let
            points = revertPoints model.points model.unit
        in
          (model, run (Render points (model.iteration - 1)))
      else
        (model, Cmd.none)
    Iterate ->
      let
          -- declare local variables for use in return
          points = model.points ++ (iteratePoints model.points)
      in
          (model, run (Render points (model.iteration + 1)))

-- TODO: learn more about Cmd msg and how to fire off RollColors without this...
run : msg -> Cmd msg
run m = Task.perform (always m) (Task.succeed ())

abs : Int -> Int
abs i =
  if i < 0 then (i * -1) else i

convertPointsToScales : List Point -> List DragonScale -> List DragonScale
convertPointsToScales points scales =
  if List.isEmpty points then
      scales
  else
      convertPointsToScales (List.drop 1 points) (scales ++ [ DragonScale (List.take 2 points) 0 ])

validatePoints : List Point -> Int -> Int -> List Point
validatePoints points tX tY =
  List.map (\p -> transformPoint p tX tY) points

revertPoints : List Point -> Int -> List Point
revertPoints points unit =
  {-
    Method:
      - Get first half of points list
      - Transform first point in list to match (unit, unit)
      - Transform all additional points using same transformation
  -}
  let
      halfPoints = List.take ((List.length points) // 2 + 1) points
      firstSectionPoint =
        case List.head halfPoints of
          Nothing ->
            Point unit unit
          Just i ->
            i
      transformX = unit - firstSectionPoint.x
      transformY = unit - firstSectionPoint.y
  in
    List.map (\p -> transformPoint p transformX transformY) halfPoints


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
      , button [ onClick Revert, style "margin-right" "0.5rem" ] [ text "Revert" ]
      , button [ onClick Reset, style "margin-right" "0.5rem" ] [ text "Reset" ]
      , text ("Iteration: " ++ (String.fromInt model.iteration))
      ]
    -- debug stuff
    -- , div [] [ text ("selected color: " ++ (getColorByIndex colors model.color)) ]
    -- , div [] [ text ("polyline points: " ++ (dragonToString model.points)) ]
    -- , div [] [ text ("max x: " ++ (String.fromInt model.maxX)) ]
    -- , div [] [ text ("min x: " ++ (String.fromInt model.minX)) ]
    -- , div [] [ text ("max y: " ++ (String.fromInt model.maxY)) ]
    -- , div [] [ text ("min y: " ++ (String.fromInt model.minY)) ]
    -- , div [] [ text ("viewbox height: " ++ (String.fromInt model.height)) ]
    -- , div [] [ text ("viewbox width: " ++ (String.fromInt model.width)) ]
    , svg [ width (String.fromInt model.width)
          , height (String.fromInt model.height)
          , viewBox ( (String.fromInt (model.unit * -1)) -- minX
                    ++ " "
                    ++ (String.fromInt (model.unit * -1)) -- minY
                    ++ " " 
                    ++ (String.fromInt (model.width + model.unit))  -- width
                    ++ " " 
                    ++ (String.fromInt (model.height + model.unit)) -- height
                    )
          , style "padding" "1rem"
          -- ] [ g [ transform "rotate(45,0,0)" ]
          ] [ g []
                (List.map viewDragonScale model.scales)
          ]
    ]

viewDragonScale : DragonScale -> Svg.Svg msg
viewDragonScale scale =
  polyline  [ fill "none"
            , stroke "black"
            , strokeWidth "3"
            , points (dragonToString scale.points)
            ] []

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
