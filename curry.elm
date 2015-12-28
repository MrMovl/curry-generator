import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Random exposing (initialSeed, int, generate, Seed)
import Time exposing (every)

main =
  StartApp.start { model = model, view = view, update = update }

base = [ "onions", "coconut milk", "tomatoes", "joghurt and creme" ]
spices = [ "pepper", "garam masala" ]
mainIngredient = [ "chicken", "lamb", "cauliflower" ]


type alias Model = 
  { base : String
  , spices : String
  , mainIngredient : String
  , seed : Seed
  }

model = 
  { base = ""
  , spices = ""
  , mainIngredient = ""
  , seed = initialSeed 42
  }

view address model =
  div []
    [ button [ onClick address Generate ] [ text "Generate recipe" ]
    , div [] [ text ("This will be you watery base: " ++ model.base) ]
    , div [] [ text ("You can use these spices: " ++ model.spices) ]
    , div [] [ text ("And add this as your main ingredient: " ++ model.mainIngredient)]
    ]


type Action = Generate


update action model =
  case action of
    Generate -> createRandomRecipe model

createRandomRecipe : Model -> Model
createRandomRecipe model = 
  { base = pickIngredient base model.seed
  , spices = pickIngredient spices model.seed
  , mainIngredient = pickIngredient mainIngredient model.seed
  , seed = getNewSeed model.seed
  }

getNewSeed : Seed -> Seed
getNewSeed oldSeed =
  let gen = Random.int 0 10
      (i, seed) = Random.generate gen oldSeed
  in seed

pickIngredient : List String -> Seed -> String
pickIngredient list oldSeed = 
  let randomIndexGenerator = Random.int 0 (List.length list - 1)
      (i, seed) = Random.generate randomIndexGenerator oldSeed
      elem = List.drop i list |> List.head
  in Maybe.withDefault "" elem