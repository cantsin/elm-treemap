module Treemap.Render where

import Text exposing (..)
import String exposing (..)
import Treemap exposing (Data, Coordinate, squarify)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (Form, rect, move, text, filled, collage, group)
import Color exposing (Color, rgba, white, hsla, toHsl)

type alias SampleData = List (String, Float)

type alias TreeMapStyle =
  {
    width : Int
  , height : Int
  , textColor : Color
  , mapColor : Color
  , maximumFontSize : Int
  }

defaultStyle =
  {
    width = 400
  , height = 200
  , textColor = white
  , mapColor = rgba 255 0 255 255
  , maximumFontSize = 20
  }

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

fontSize : Float -> Float -> Float -> Int -> Float
fontSize average width height maximum =
  let r = width * height |> sqrt in
  min (r / average) (Basics.toFloat maximum)

draw : TreeMapStyle -> Float -> Coordinate -> String -> Float -> Form
draw style average coord title gradient =
  let width = Basics.toFloat style.width
      height = Basics.toFloat style.height
      w = coord.x2 - coord.x1
      h = coord.y2 - coord.y1
      -- translate from top-left coordinates to center of element
      x = coord.x1 + (w / 2) - (width / 2)
      y = (height / 2) - (h / 2) - coord.y1
      hsl = Color.toHsl style.mapColor
      c = { hsl | lightness <- gradient }
      color = hsla c.hue c.saturation c.lightness c.alpha
      size = fontSize average w h style.maximumFontSize
      t = Text.fromString title
        |> Text.height size
        |> Text.color style.textColor
        |> text
      g = rect w h
        |> filled color in
  group <| List.map (move (x, y)) [g, t]

averageLabelSize : List String -> Float
averageLabelSize labels =
  let n = List.map String.length labels
        |> List.sum
        |> Basics.toFloat
      d = List.length labels
        |> Basics.toFloat in
  n / d

treemap : SampleData -> TreeMapStyle -> Element
treemap data style =
  let width = Basics.toFloat style.width
      height = Basics.toFloat style.height
      area = width * height
      (titles, values) = extract data
      normalizedData = normalize area values
      average = averageLabelSize titles
      total = List.sum values
      gradients = List.map (\v -> v / total) values
      result = squarify normalizedData { x = 0, y = 0, width = width, height = height }
      rectangles = List.map3 (draw style average) result titles gradients in
  collage style.width style.height rectangles
