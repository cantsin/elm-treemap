import Graphics.Element exposing (..)

type alias Data = List Float

type alias Rectangle =
  {
    x : Float
  , y : Float
  , width : Float
  , height: Float
  }

type alias Coordinate =
  {
    x1 : Float
  , y1 : Float
  , x2 : Float
  , y2 : Float
  }

type alias Area = Float

shortestEdge : Rectangle -> Float
shortestEdge r = min r.height r.width

improvesRatio : List Float -> Float -> Float -> Bool
improvesRatio rows next length = True

coordinates : List Float -> Rectangle -> List Coordinate
coordinates rows r =
  let width = List.sum rows / r.height
      height = List.sum rows / r.width in
  if r.width >= r.height then
    let offsets = List.scanl (\r y -> y + r / width) r.y rows in
    List.map2 (\row offset -> { x1 = r.x
                              , y1 = offset
                              , x2 = r.x + width
                              , y2 = offset + row / width
                              }) rows offsets
  else
    let offsets = List.scanl (\x r -> x + r / width) r.x rows in
    List.map2 (\row offset -> { x1 = offset
                              , y1 = r.y
                              , x2 = offset + row / height
                              , y2 = r.y + height}) rows offsets

squarify : Data -> List Float -> Rectangle -> List Coordinate
squarify values rows container =
  case List.head values of
    Just datapoint ->
      let length = shortestEdge container
          remaining = case List.tail values of
                        Just x -> x
                        Nothing -> [] in
      if improvesRatio rows datapoint length then
        squarify remaining (datapoint :: rows) container
      else
        let c = { x = 0, y = 0, height = 0, width = 0 } in
        squarify values [] c |> (++) (coordinates rows container)
    Nothing ->
      coordinates rows container

-- testing purposes only.

type alias SampleData = List (String, Float)

extract : SampleData -> Data
extract datum = List.map snd datum |> List.sort |> List.reverse

normalize : Float -> Data -> Data
normalize area values =
  let total = List.sum values
      multiplier = area / total in
    List.map (\v -> v * multiplier) values

main = show <| (extract testData |> normalize (10*20))

testData : SampleData
testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]
