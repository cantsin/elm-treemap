module Main where

import Treemap.Render exposing (SampleData, treemap, defaultStyle)
import Graphics.Element exposing (..)
import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed, initialSeed, generate, int)

type alias Model =
  {
    data : SampleData
  , seed : Seed
  }

type Action = Add
            | Reset

newValue : Seed -> (Int, Seed)
newValue = generate <| int 1 10

generateOne : (Int, Int, Seed) -> (String, Float)
generateOne (n, v, seed) = ("Area " ++ toString n, Basics.toFloat v)

generateModel : Seed -> Model
generateModel seed =
  let (n, seed0) = generate (int 5 10) seed
      -- generate a series of seeds that depend on the previous seed.
      -- gather up the counter and value while we're at it.
      values = List.scanl (\c (_, _, seed) ->
                             let (v, nextSeed) = newValue seed in
                             (c, v, nextSeed)
                          ) (0, n, seed0) [1..n]
      -- we don't want the first value, as that is what we used to
      -- seed the sequence above.
      data = List.map generateOne <| List.drop 1 values
      last = case List.head <| List.reverse values of
               Just (_, _, seed) -> seed
               Nothing -> seed0 in
  { data = data, seed = last }

update : Action -> Model -> Model
update action model =
  case action of
    Add ->
      let n = List.length model.data + 1
          (value, nextSeed) = newValue model.seed
          v = generateOne (n, value, nextSeed) in
      {
        data = v :: model.data
      , seed = nextSeed
      }
    Reset ->
      generateModel model.seed

view : Signal.Address Action -> Model -> Html
view address model =
  div []
        [ div [] [ treemap model.data defaultStyle |> fromElement ]
        , div [ debugStyle ] [ text <| toString model.data ]
        , div [] [ button [ onClick address Add] [ text "Add a new value" ]
                 , button [ onClick address Reset] [ text "Reset" ] ]
        ]

debugStyle : Attribute
debugStyle =
  style
    [ ("font-size", "14px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("text-align", "center")
    ]

testData = [ ("Area 1", 6)
           , ("Area 2", 6)
           , ("Area 3", 4)
           , ("Area 4", 3)
           , ("Area 5", 2)
           , ("Area 6", 2)
           , ("Area 7", 1)
           ]

main = start
       {
         model = { data = testData, seed = initialSeed 0 }
       , update = update
       , view = view
       }

-- TODO add README.md (link to pdf)
