module Main where

import Text exposing (..)
import Treemap exposing (Data, Coordinate, squarify)
import Graphics.Element exposing (show)
import Graphics.Collage exposing (Form, rect, move, text, defaultLine, filled, collage, group)
import Color exposing (rgba, white)

-- testing purposes only.

type alias SampleData = List (String, Float)

style = { defaultStyle | color <- white }

extract : SampleData -> (List String, Data)
extract datum =
  List.sortBy snd datum
    |> List.reverse
    |> List.unzip

normalize : Float -> Data -> Data
normalize area values =
  let total = List.sum values
      multiplier = area / total in
    List.map (\v -> v * multiplier) values

draw : Coordinate -> String -> Form
draw c title =
  let w = c.x2 - c.x1
      h = c.y2 - c.y1
      -- translate from top-left coordinates to center of element
      x = c.x1 + (w / 2) - (width / 2)
      y = (height / 2) - (h / 2) - c.y1
      -- temporary colors, for now
      color = rgba 255 (c.y2 |> round) 255 255
      t = text <| Text.color white (Text.height (h / 4) (Text.fromString title))
      g = rect w h in
  group [move (x, y) <| filled color g, move(x, y) t]

main =
  let area = width * height
      (titles, values) = extract testData
      data = normalize area values
      result = squarify data { x = 0, y = 0, width = width, height = height }
      rectangles = List.map2 draw result titles in
  collage width height rectangles

width = 400
height = 200

testData : SampleData
testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]
