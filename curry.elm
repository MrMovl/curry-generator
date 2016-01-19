import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Random exposing (initialSeed, int, generate, Seed)
import Random.Array exposing (shuffle)
import List exposing (head, foldl)
import Array exposing (fromList, slice, toList)
import String exposing (length)

main =
  StartApp.start { model = model, view = view, update = update }

base = fromList [ "onions", "coconut milk", "tomatoes", "joghurt and creme" ]
spices = fromList [ "Pfeffer", "Garam Masala", "Kumin", "Kardamom", "Senfsamen", "Zimt", "Gewürznelken", "Chilli", "Ingwer" ]
mainIngredient = fromList [ "chicken", "lamb", "cauliflower", "Aubergine" ]


type alias Model = 
  { base : Maybe String
  , spices : List String
  , mainIngredient : Maybe String
  , seed : Seed
  }

model = 
  { base = ""
  , spices = []
  , mainIngredient = ""
  , seed = initialSeed 42
  }

view address model =
  div []
    [ button [ onClick address Generate ] [ text "Generate recipe" ]
    , div [] [ text ("This will be you watery base: " ++ model.base) ]
    , div [] [ text ("You can use these spices: " ++ foldl (++) "" model.spices) ]
    , div [] [ text ("And add this as your main ingredient: " ++ model.mainIngredient) ]
    ]


type Action = Generate


update action model =
  case action of
    Generate -> createRandomRecipe model

createRandomRecipe : Model -> Model
createRandomRecipe model = 
  { base = head (pick base model.seed 1)
  , spices = pick spices model.seed 3
  , mainIngredient = head (pick mainIngredient model.seed 1)
  , seed = getNewSeed model
  }

-- uses semi-random data to generate a new seed for the next recipe
getNewSeed : Model -> Seed
getNewSeed model =
  let 
    gen = Random.int (length model.base) (length model.mainIngredient)
    (i, seed) = Random.generate gen model.seed
  in
    seed

pick : Array.Array String -> Seed -> Int -> List String
pick input seed count =
  let
    (shuffledArray, _) = shuffle seed input
  in
    toList (slice 0 count shuffledArray)