module CurryGenerator where

import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Random exposing (initialSeed, int, generate, Seed)
import Random.Array as ShuffleArray
import List exposing (head, foldl)
import Array exposing (fromList, slice, toList)
import String exposing (length, dropRight)

-- StartApp
main =
  StartApp.start { model = model, view = view, update = update }

-- My lists the ingredients
base = 
  fromList [ "deglazed onions", "coconut milk", "tomatoes", "joghurt and creme" ]

spices = 
  fromList [ "pepper", "Garam Masala", "cumin", "cardamom", "mustard seeds", "cinnamon", "clove", "chilli", "ginger" ]

mainIngredient = 
  fromList [ "chicken", "lamb", "cauliflower", "aubergine" ]

--------------------------------------------------------------------------

-- Model holds the current recipe and a seed
type alias Model = 
  { base : String
  , spices : String
  , mainIngredient : String
  , seed : Seed
  }

-- initial model
model = 
  { base = ""
  , spices = ""
  , mainIngredient = ""
  , seed = initialSeed 42
  }

--------------------------------------------------------------------------

-- simple view, which needs to get prettier
view address model =
  div []
    [ button [ onClick address Generate ] [ text "Generate recipe" ]
    , div [] [ text ("This will be you watery base: " ++ model.base) ]
    , div [] [ text ("You can use these spices: " ++ model.spices) ]
    , div [] [ text ("And add this as your main ingredient: " ++ model.mainIngredient) ]
    ]

--------------------------------------------------------------------------

type Action = Generate

-- update is simple
update action model =
  case action of
    Generate -> createRandomRecipe model

--------------------------------------------------------------------------

-- this creates a new model, but delegates all of the heavy lifting
createRandomRecipe : Model -> Model
createRandomRecipe model = 
  { base = pick base model.seed 1
  , spices = pick spices model.seed 3
  , mainIngredient = pick mainIngredient model.seed 1
  , seed = getNewSeed model
  }

--------------------------------------------------------------------------

-- generalized function, the only one which has nothing to do with food ;)
pick : Array.Array String -> Seed -> Int -> String
pick input seed count =
  let
    (shuffledArray, _) = ShuffleArray.shuffle seed input 
    result = shuffledArray |> toList |> List.take count
  in
    String.join ", " result

--------------------------------------------------------------------------

-- uses semi-random data to generate a new seed for the next recipe
getNewSeed : Model -> Seed
getNewSeed model =
  let 
    gen = Random.int (length model.base) (length model.mainIngredient)
    (_, seed) = Random.generate gen model.seed
  in
    seed