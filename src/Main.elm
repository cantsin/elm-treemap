module Main where

import Graphics.Element exposing (show)
import Treemap exposing (Data, Coordinate, squarify)
import Graphics.Collage exposing (Form, rect, move, filled, collage)
import Color exposing (rgba)

-- testing purposes only.

type alias SampleData = List (String, Float)

extract : SampleData -> Data
extract datum = List.map snd datum |> List.sort |> List.reverse

normalize : Float -> Data -> Data
normalize area values =
  let total = List.sum values
      multiplier = area / total in
    List.map (\v -> v * multiplier) values

draw : Coordinate -> Form
draw c =
  let w = c.x2 - c.x1
      h = c.y2 - c.y1
      -- translate from top-left coordinates to center of element
      x = c.x1 + (w / 2) - (width / 2)
      y = (height / 2) - (h / 2) - c.y1
      -- temporary colors, for now
      color = rgba 255 (c.y2 |> round) 255 255
      g = rect w h in
  move (x, y) <| filled color g

main =
  let area = width * height
      data = extract testData |> normalize area
      result = squarify data { x = 0, y = 0, width = width, height = height }
      minimum = 1
      maximum = 6
      rectangles = List.map draw result in
  collage width height rectangles

width = 200
height = 100

testData : SampleData
testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]
