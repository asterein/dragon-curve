module RainbowDragon exposing (..)

import Array
import Browser
import Colors exposing (colors, getColorByIndex, getRandColorIndex)
import Html exposing (Html, div, text, button, textarea)
import Html.Events exposing (onClick, onInput, on)
import Html.Attributes exposing (type_, placeholder, value, class, attribute)
import Random
import Svg exposing (svg, polyline, g, defs, linearGradient, stop)
import Svg.Attributes exposing  ( fill
                                , stroke
                                , strokeWidth
                                , strokeLinecap
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
import Task

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
  , debug : Bool
  , isLoading : Bool
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

initialDragonScale : Int -> DragonScale
initialDragonScale i = DragonScale [ Point i i, Point 0 i ] 0

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
      model = Model [] [] 0 0 0 0 0 0 0 0 False False
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
  | ColorGeneration (List DragonScale) Int
  | Iterate
  | Revert
  | Reset
  | ToggleDebug
  | Loaded

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded ->
      ({ model | isLoading = False }, Cmd.none)
    ToggleDebug ->
      ({ model | debug = not model.debug }, Cmd.none)
    Reset ->
      let
          resetModel = dragonEgg 15
      in
        ( { resetModel | debug = model.debug }
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
            scales = convertPointsToScales validatedPoints []
      in
          ({ model  | points = validatedPoints
                    , scales = []
                    , height = height
                    , width = width
                    , maxX = maxX
                    , minX = minX
                    , maxY = maxY
                    , minY = minY
                    , iteration = iteration
                    }
          , Random.generate (ColorGeneration scales) getRandColorIndex
          )
    ColorGeneration scales index ->
      if List.isEmpty scales then
        (model, Cmd.none)
      else
        let
          initScale = getFirstDragonScale scales model.unit
        in
          ( { model | scales = model.scales ++ [ DragonScale initScale.points index ] }
          , Random.generate (ColorGeneration (List.drop 1 scales)) getRandColorIndex
          )
    Revert ->
      if model.iteration <= 1 then
        update Reset model
      else if model.iteration > 1 then
        update (Render (revertPoints model.points model.unit) (model.iteration - 1)) model
      else
        (model, Cmd.none)
    Iterate ->
      if model.iteration < 11 then
        update (Render (model.points ++ (iteratePoints model.points)) (model.iteration + 1)) model
        -- ({ model | isLoading = True }, Cmd.none)
      else
        (model, Cmd.none)


colorScale : DragonScale -> Int -> DragonScale
colorScale scale i =
  { scale | color = i }

getFirstDragonScale : List DragonScale -> Int -> DragonScale
getFirstDragonScale scales unit =
  case Array.get 0 (Array.fromList (List.take 1 scales)) of
    Nothing ->
      DragonScale [ Point unit unit, Point 0 unit ] 0
    Just s ->
      s

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
    div [ styleWrapper ]
    [ div [ styleIterationCount ] [ button [ onClick Revert, styleBtn ] [ text "«" ]
                                  , div [] [ text ("Iteration " ++ (iterationCountToString model.iteration)) ]
                                  , button [ onClick Iterate, styleBtn ] [ text "»" ]
                                  ]
    , div [ styleControls ] [ button [ onClick Reset, styleBtn ] [ text "reset" ] ]
    , viewDebug model
    , viewDragon model
    ]

viewDragon : Model -> Html Msg
viewDragon model =
  if model.isLoading then
    div [] [ text "loading" ]
  else
    div []  [ svg [ width (String.fromInt model.width)
            , height (String.fromInt model.height)
            , viewBox (getViewBoxAttr model)
            , styleSvg
            -- ] [ g [ transform "rotate(45,0,0)" ]
            ] [ g []
                  (List.map viewDragonScale model.scales)
              ]
            ]

iterationCountToString : Int -> String
iterationCountToString i =
  if i < 10 then
    "0" ++ (String.fromInt i)
  else
    String.fromInt i

getViewBoxAttr : Model -> String
getViewBoxAttr model =
  ( (String.fromInt (model.unit * -1)) -- minX
  ++ " " ++ (String.fromInt (model.unit * -1)) -- minY
  ++ " " ++ (String.fromInt (model.width + model.unit))  -- width
  ++ " " ++ (String.fromInt (model.height + model.unit)) -- height
  )


viewDebug : Model -> Html Msg
viewDebug model =
  if not model.debug then
    button [ onClick ToggleDebug, styleDebugBtn ] [ text "debug" ]
  else
    div [ styleDebugPanel ]
      [ div [] [ button [ onClick ToggleDebug, styleDebugPanelCloseBtn ] [ text "x" ] ]
      , div [] [ text ("isLoading: " ++ (if model.isLoading then "1" else "0")) ]
      , div [] [ text ("(min, max) x: (" ++ (String.fromInt model.minX) ++ ", " ++ (String.fromInt model.maxX) ++ ")") ]
      , div [] [ text ("(min, max) y: (" ++ (String.fromInt model.minY) ++ ", " ++ (String.fromInt model.maxY) ++ ")") ]
      , div [] [ text ("viewbox height: " ++ (String.fromInt model.height)) ]
      , div [] [ text ("viewbox width: " ++ (String.fromInt model.width)) ]
      , div [] [ text "points:" ]
      , div [] [ textarea [ styleDebugPoints ] [ text (dragonToString model.points) ] ]
      ]

viewDragonScale : DragonScale -> Svg.Svg msg
viewDragonScale scale =
  polyline  [ fill "none"
            , stroke (getColorByIndex colors scale.color)
            , strokeLinecap "square"
            -- , strokeLinecap "round"
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

-- STYLES

toStyle : String -> Html.Attribute Msg
toStyle styling = attribute "style" styling

styleBtn = toStyle """
  font-family: inherit;
  font-size: inherit;
  border: none;
  background: none;
  color: inherit;
  cursor: pointer;
  margin: 0 0.5rem;
  font-variant: inherit;
  font-style: inherit;
  font-weight: inherit;
"""

styleWrapper = toStyle """
  margin: 0 auto;
  padding: 0;
  font-family: sans-serif;
  min-width: 100vw;
  min-height: 100vh;
  box-sizing: border-box;
  background: #111;
  color: #eee;
  overflow: hidden;
"""

styleDebugBtn = toStyle """
  display: inline-block;
  position: fixed;
  bottom: 0.5rem;
  right: 0.5rem;
  cursor: pointer;
  background: transparent;
  border: none;
  color: inherit;
"""

styleDebugPanel = toStyle """
  display: block;
  font-family: monospace;
  font-size: 0.8rem;
  padding: 1rem;
  position: fixed;
  bottom: 0.5rem;
  right: 0.5rem;
  background: rgba(50,50,50,0.7);
  backdrop-filter: blur(2px);
  border-radius: 0.15rem;
"""

styleDebugPanelCloseBtn = toStyle """
  display: grid;
  height: 20px;
  width: 20px;
  color: #fff;
  border: none;
  background: maroon;
  border-radius: 100%;
  font-weight: bold;
  cursor: pointer;
  position: absolute;
  top: -7px;
  left: -7px;
  padding: 0;
  margin: 0;
"""

styleDebugPoints = toStyle """
  display: block;
  background: transparent;
  height: 50px;
  color: inherit;
  border: 1px solid #eee;
  resize: none;
  font-size: inherit;
"""

styleIterationCount = toStyle """
  font-size: 2rem;
  text-align: center;
  padding: 1.5rem 0 0.25rem 0;
  margin: 0 auto;
  font-variant: small-caps;
  letter-spacing: 0.1em;
  display: flex;
  align-content: center;
  justify-content: center;
"""

styleControls = toStyle """
  margin: 0 auto 1rem auto;
  display: block;
  max-width: 500px;
  text-align: center;
  font-variant: small-caps;
  font-size: 0.8rem;
  font-style: italic;
"""

styleSvg = toStyle """
  margin: 0 auto;
  display: block;
"""
