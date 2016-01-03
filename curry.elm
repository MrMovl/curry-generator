import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Random exposing (initialSeed, int, generate, Seed)
import Time exposing (every)
import List exposing (foldl)
import String exposing (length)

main =
  StartApp.start { model = model, view = view, update = update }

base = [ "onions", "coconut milk", "tomatoes", "joghurt and creme" ]
spices = [ "Pfeffer", "Garam Masala", "Kumin", "Kardamom", "Senfsamen", "Zimt", "Gewürznelken", "Chilli", "Ingwer" ]
mainIngredient = [ "chicken", "lamb", "cauliflower", "Aubergine" ]


type alias Model = 
  { base : String
  , spices : List String
  , mainIngredient : String
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
  { base = pickIngredient base model.seed
  , spices = pickStuff spices model.seed 3
  , mainIngredient = pickIngredient mainIngredient model.seed
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

pickStuff : List String -> Seed -> Int -> List String
pickStuff list seed count =
  if 
    count == 1
  then
    pickIngredient list seed :: []
  else
    pickIngredient list seed :: pickStuff list seed (count - 1)


pickIngredient : List String -> Seed -> String
pickIngredient list seed = 
  let 
    randomIndexGenerator = Random.int 0 (List.length list - 1)
    (i, s) = Random.generate randomIndexGenerator seed
    elem = List.drop i list |> List.head
  in
    (Maybe.withDefault "" elem)