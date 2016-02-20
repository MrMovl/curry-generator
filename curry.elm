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
  , storedRecipe : Recipe
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
  , storedRecipe = initialRecipe
  , seed = Random.initialSeed newDate
  }

--------------------------------------------------------------------------

-- simple view, which needs to get prettier
myView address {recipe, storedRecipe, seed} =
  Html.div [ generalStyle ]
    [ Html.div [ topStyle ] 
      [ Html.text explanation
      , Html.button [ Events.onClick address Generate ] [ Html.text "Generate recipe" ]
      , Html.button [ Events.onClick address Store ] [ Html.text "Store recipe" ]
      ]
    , Html.div []
      [ Html.div [ leftColumn ]  [ recipeView recipe ]
      , Html.div [ rightColumn ] [ recipeView storedRecipe ]
      ]
    ]

recipeView : Recipe -> Html.Html      
recipeView recipe =
  Html.div [recipeStyle]
    [ Html.div [] [ Html.text ("This will be you watery base: " ++ recipe.base) ]
    , Html.div [] [ Html.text ("You can use these spices: " ++ recipe.spices) ]
    , Html.div [] [ Html.text ("And add this as your main ingredient: " ++ recipe.mainIngredient) ]
    ]

--------------------------------------------------------------------------
-- simple port to get the current time in here, so this is a pure input port
port newDate : Int

type Action = Generate | Store

-- update is simple
myUpdate action model =
  case action of
    Generate -> createRandomRecipe model
    Store -> storeRecipe model

--------------------------------------------------------------------------
storeRecipe : Model -> Model
storeRecipe {recipe, storedRecipe, seed} =
  { recipe = initialRecipe
  , storedRecipe = recipe
  , seed = seed
  }


-- this creates a new model, but delegates all of the heavy lifting
createRandomRecipe : Model -> Model
createRandomRecipe {recipe, storedRecipe, seed} =
  let
    (newBase, seedOne) = pick base 1 seed
    (newSpices, seedTwo) = pick spices 3 seedOne
    (newMainIngredient, seedThree) = pick mainIngredient 1 seedTwo
    newRecipe = { base = newBase
      , spices = newSpices
      , mainIngredient = newMainIngredient
      }
  in
    { recipe = newRecipe
    , storedRecipe = storedRecipe
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

generalStyle : Html.Attribute
generalStyle =
  Attr.style
    [ ("font-size", "16px")
    , ("font-family", "monospace")
    ]


recipeStyle : Html.Attribute
recipeStyle =
  Attr.style
    [ ("display", "inline-block")
    , ("width", "100%")
    , ("text-align", "left")
    ]

leftColumn : Html.Attribute
leftColumn =
  Attr.style
    [ ("float", "left")
    , ("width", "50%")
    ]

rightColumn : Html.Attribute
rightColumn =
  Attr.style
    [ ("float", "right")
    , ("width", "50%")
    ]

topStyle : Html.Attribute
topStyle =
  Attr.style
    [ ("height", "100px")
    , ("width", "100%")
    ]