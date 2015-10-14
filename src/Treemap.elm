module Treemap where

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

ratio : List Float -> Float -> Float
ratio rows length =
  case (List.minimum rows, List.maximum rows) of
    (Just minimum, Just maximum) ->
      let sum = List.sum rows
          l2 = length * length
          s2 = sum * sum in
      max ((l2 * maximum) / s2) (s2 / (l2 * minimum))
    _ ->
      0.0

betterRatio : List Float -> Float -> Float -> Bool
betterRatio rows next length =
  case List.length rows of
    0 -> True
    _ ->
      let current = ratio rows length
          new = ratio (List.append rows [next]) length in
      current >= new

cut : Rectangle -> Float -> Rectangle
cut r area =
  if r.width >= r.height then
    let areaWidth = area / r.height
        newWidth = r.width - areaWidth in
    { x = r.x + areaWidth
    , y = r.y
    , width = newWidth
    , height = r.height }
  else
    let areaHeight = area / r.width
        newHeight = r.height - areaHeight in
    { x = r.x
    , y = r.y + areaHeight
    , width = r.width
    , height = newHeight }

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
      let length = min container.height container.width in
      if betterRatio rows datapoint length then
        let remaining = case List.tail values of
                          Just x -> x
                          Nothing -> [] in
        squarify remaining (List.append rows [datapoint]) container
      else
        let c = cut container <| List.sum rows
            result = coordinates rows container in
        squarify values [] c
          |> (++) result
    Nothing ->
      coordinates rows container
