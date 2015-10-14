module Main where

import Text exposing (..)
import String exposing (..)
import Treemap exposing (Data, Coordinate, squarify)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (Form, rect, move, text, defaultLine, filled, collage, group)
import Color exposing (rgba, white)

type alias SampleData = List (String, Float)

maximumFontSize = 20

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

fontSize : Float -> Float -> Float -> Float
fontSize average width height =
  let r = width * height |> sqrt in
  min (r / average) maximumFontSize

draw : Float -> Float -> Float -> Coordinate -> String -> Form
draw width height average coord title =
  let w = coord.x2 - coord.x1
      h = coord.y2 - coord.y1
      -- translate from top-left coordinates to center of element
      x = coord.x1 + (w / 2) - (width / 2)
      y = (height / 2) - (h / 2) - coord.y1
      -- temporary colors, for now
      color = rgba 255 (coord.y2 |> round) 255 255
      size = fontSize average w h
      t = Text.fromString title
        |> Text.height size
        |> Text.color white
        |> text
      g = rect w h in
  group [move (x, y) <| filled color g, move(x, y) t]

averageLabelSize : List String -> Float
averageLabelSize labels =
  let n = List.map String.length labels
        |> List.sum
        |> Basics.toFloat
      d = List.length labels
        |> Basics.toFloat in
  n / d

treemap : Int -> Int -> SampleData -> Element
treemap w h data =
  let width = Basics.toFloat w
      height = Basics.toFloat h
      area = width * height
      (titles, values) = extract data
      normalizedData = normalize area values
      average = averageLabelSize titles
      result = squarify normalizedData { x = 0, y = 0, width = width, height = height }
      rectangles = List.map2 (draw width height average) result titles in
  collage w h rectangles

-- testing purposes only.

main : Element
main = treemap 400 200 testData

testData : SampleData
testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]
