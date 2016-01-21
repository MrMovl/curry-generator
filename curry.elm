import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Random exposing (initialSeed, int, generate, Seed)
import Random.Array exposing (shuffle)
import List exposing (head, foldl)
import Array exposing (fromList, slice, toList)
import String exposing (length, dropRight)
import Maybe exposing (withDefault)

-- StartApp
main =
  StartApp.start { model = model, view = view, update = update }

-- My lists the ingredients
base = 
  fromList [ "onions", "coconut milk", "tomatoes", "joghurt and creme" ]

spices = 
  fromList [ "Pfeffer", "Garam Masala", "Kumin", "Kardamom", "Senfsamen", "Zimt", "Gewürznelken", "Chilli", "Ingwer" ]

mainIngredient = 
  fromList [ "chicken", "lamb", "cauliflower", "Aubergine" ]

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

-- simple view, which needs to get prettier
view address model =
  div []
    [ button [ onClick address Generate ] [ text "Generate recipe" ]
    , div [] [ text ("This will be you watery base: " ++ model.base) ]
    , div [] [ text ("You can use these spices: " ++ model.spices) ]
    , div [] [ text ("And add this as your main ingredient: " ++ model.mainIngredient) ]
    ]


type Action = Generate

-- update is simple
update action model =
  case action of
    Generate -> createRandomRecipe model

-- this creates a new model, but delegates all of the heavy lifting
createRandomRecipe : Model -> Model
createRandomRecipe model = 
  { base = pick base model.seed 1
  , spices = pick spices model.seed 3
  , mainIngredient = pick mainIngredient model.seed 1
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

-- generalized function, the only one which has nothing to do with food ;)
pick : Array.Array String -> Seed -> Int -> String
pick input seed count =
  let
    (shuffledArray, _) = shuffle seed input
    result = slice 0 count shuffledArray |> toList
  in
    if count == 1 then 
      head result |> withDefault ""
    else 
      addCommas result

-- the menial task of prettifying a list of stuff into a string...
addCommas : List String -> String
addCommas =
  String.join ", "
