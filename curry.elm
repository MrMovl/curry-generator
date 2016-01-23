module CurryGenerator where

import Html
import Html.Attributes as Attr
import Html.Events as Events
import StartApp.Simple as StartApp
import Random
import Random.Array as ShuffleArray
import Array
import String

-- StartApp
main =
  StartApp.start { model = model, view = view, update = update }

-- My lists the ingredients
base = 
  Array.fromList [ "deglazed onions", "coconut milk", "tomatoes", "joghurt and creme" ]

spices = 
  Array.fromList [ "pepper", "Garam Masala", "cumin", "cardamom", "mustard seeds", "cinnamon", "clove", "chilli", "ginger" ]

mainIngredient = 
  Array.fromList [ "chicken", "lamb", "cauliflower", "aubergine" ]

--------------------------------------------------------------------------

-- Model holds the current recipe and a seed
type alias Model = 
  { base : String
  , spices : String
  , mainIngredient : String
  , seed : Random.Seed
  }

-- initial model
model = 
  { base = ""
  , spices = ""
  , mainIngredient = ""
  , seed = Random.initialSeed 42
  }

--------------------------------------------------------------------------

-- simple view, which needs to get prettier
view address model =
  Html.div []
    [ Html.button [ Events.onClick address Generate ] [ Html.text "Generate recipe" ]
    , Html.div [] [ Html.text ("This will be you watery base: " ++ model.base) ]
    , Html.div [] [ Html.text ("You can use these spices: " ++ model.spices) ]
    , Html.div [] [ Html.text ("And add this as your main ingredient: " ++ model.mainIngredient) ]
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
pick : Array.Array String -> Random.Seed -> Int -> String
pick input seed count =
  let
    (shuffledArray, _) = ShuffleArray.shuffle seed input 
    result = shuffledArray |> Array.toList |> List.take count
  in
    String.join ", " result

--------------------------------------------------------------------------

-- uses semi-random data to generate a new seed for the next recipe
getNewSeed : Model -> Random.Seed
getNewSeed model =
  let 
    gen = Random.int (String.length model.base) (String.length model.mainIngredient)
    (_, seed) = Random.generate gen model.seed
  in
    seed