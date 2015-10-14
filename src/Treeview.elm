import Graphics.Element exposing (..)

type alias SampleData = List (String, Float)

type alias Rectangle =
  {
    x : Int
  , y : Int
  , width : Int
  }

type alias Area = Float

testData : SampleData
testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]

extract : SampleData -> List Float
extract datum = List.map snd datum |> List.sort |> List.reverse

main = show <| extract testData
