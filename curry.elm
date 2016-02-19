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
  StartApp.start { model = myModel, view = myView, update = myUpdate }

-- My lists the ingredients
base = 
  Array.fromList [ "deglazed onions", "coconut milk", "tomatoes", "joghurt and creme" ]

spices = 
  Array.fromList [ "pepper", "Garam Masala", "cumin", "cardamom", "mustard seeds", "cinnamon", "clove", "chilli", "ginger" ]

mainIngredient = 
  Array.fromList [ "chicken", "lamb", "cauliflower", "aubergine" ]

explanation = 
  "Roast your main ingredient to your liking and put it aside. 
  Then roast the spices for a short while and deglaze them with your watery base. 
  After cooking for a short while add your main ingredient and cook until it is tender.
  Serve with rice or fitting bread."


-- Model holds the current recipe and a seed
type alias Recipe =
  { base : String
  , spices : String
  , mainIngredient : String
  }

--------------------------------------------------------------------------

-- Model holds the current recipe and a seed
type alias Model = 
  { recipe : Recipe
  , seed : Random.Seed
  }

-- initial model

initialRecipe = 
  { base = ""
  , spices = ""
  , mainIngredient = ""
  }

myModel = 
  { recipe = initialRecipe
  , seed = Random.initialSeed newDate
  }

--------------------------------------------------------------------------

-- simple view, which needs to get prettier
myView address model =
  Html.div []
    [ Html.text explanation
    , Html.button [ Events.onClick address Generate ] [ Html.text "Generate recipe" ]
    --, Html.button [ Events.onClick address Store ] [ Html.text "Store recipe" ]
    , recipeView model ]
    
recipeView {recipe, seed} =
  Html.div []
    [ Html.div [] [ Html.text ("This will be you watery base: " ++ recipe.base) ]
    , Html.div [] [ Html.text ("You can use these spices: " ++ recipe.spices) ]
    , Html.div [] [ Html.text ("And add this as your main ingredient: " ++ recipe.mainIngredient) ]
    ]

--------------------------------------------------------------------------
-- simple port to get the current time in here, so this is a pure input port
port newDate : Int

type Action = Generate --| Store

-- update is simple
myUpdate action model =
  case action of
    Generate -> createRandomRecipe model.seed
--    Store -> storeRecipe model

--------------------------------------------------------------------------

-- this creates a new model, but delegates all of the heavy lifting
createRandomRecipe : Random.Seed -> Model
createRandomRecipe initialSeed =
  let
    (newBase, seedOne) = pick base 1 initialSeed
    (newSpices, seedTwo) = pick spices 3 seedOne
    (newMainIngredient, seedThree) = pick mainIngredient 1 seedTwo
    newRecipe = { base = newBase
      , spices = newSpices
      , mainIngredient = newMainIngredient
      }
  in
    { recipe = newRecipe
    , seed = seedThree
    }

--------------------------------------------------------------------------

-- generalized function, the only one which has nothing to do with food ;)
pick : Array.Array String -> Int -> Random.Seed -> (String, Random.Seed)
pick input count seed =
  let
    (shuffledArray, newSeed) = ShuffleArray.shuffle seed input 
    result = shuffledArray |> Array.toList |> List.take count
    prettyResult = String.join ", " result
  in
    (prettyResult, newSeed)
--------------------------------------------------------------------------

-- uses semi-random data to generate a new seed for the next recipe
getNewSeed : Model -> Random.Seed
getNewSeed {recipe, seed} =
  let
    gen = Random.int (String.length recipe.base) (String.length recipe.mainIngredient)
    (_, seed) = Random.generate gen seed
  in
    seed

