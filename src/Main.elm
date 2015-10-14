module Main where

import Graphics.Element exposing (show)
import Treemap exposing (Data, squarify)

-- testing purposes only.

type alias SampleData = List (String, Float)

extract : SampleData -> Data
extract datum = List.map snd datum |> List.sort |> List.reverse

normalize : Float -> Data -> Data
normalize area values =
  let total = List.sum values
      multiplier = area / total in
    List.map (\v -> v * multiplier) values

main =
  let area = (20*10)
      data = extract testData |> normalize area
      result = squarify data [] { x = 0, y = 0, width = 20, height = 10 } in
      show result

testData : SampleData
testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]
