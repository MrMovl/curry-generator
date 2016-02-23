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
  Html.ul []
    [ Html.li [] [ Html.text "Roast your main ingredient to your liking and put it aside" ]
    , Html.li [] [ Html.text "Then roast the spices for a short while and deglaze them with your watery base" ]
    , Html.li [] [ Html.text "After cooking for a short while add your main ingredient and cook until it is tender" ]
    , Html.li [] [ Html.text "Serve with rice or bread" ]
    ]

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
  , storedRecipes : List Recipe
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
  , storedRecipes = [] 
  , seed = Random.initialSeed newDate
  }

--------------------------------------------------------------------------
-- simple view, which needs to get prettier
myView address {recipe, storedRecipes, seed} =
  Html.div [ generalStyle ]
    [ Html.div [ topStyle ] 
      [ Html.h3 [] [ Html.text "Curry-Generator" ]
      , explanation
      , Html.hr [] []
      , Html.div [] 
        [ Html.button [ Events.onClick address Generate, buttonStyle ] [ Html.text "Generate recipe" ]
        , Html.button [ Events.onClick address Store, buttonStyle ] [ Html.text "Store recipe" ]
        ]
      ]
    , Html.div [ recipeContainerStyle ]
      [ Html.div [ currentRecipeStyle ]  [ recipeView recipe ]
      , Html.hr [] []
      , Html.div [ recipeStorageStyle ] ( List.map recipeView storedRecipes )
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
storeRecipe {recipe, storedRecipes, seed} =
  { recipe = initialRecipe
  , storedRecipes = buildRecipeStorage recipe storedRecipes
  , seed = seed
  }

buildRecipeStorage recipe storage =
  if recipe.base == ""
    then storage
    else recipe :: storage

-- this creates a new model, but delegates all of the heavy lifting
createRandomRecipe : Model -> Model
createRandomRecipe {recipe, storedRecipes, seed} =
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
    , storedRecipes = storedRecipes
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
    , ("width", "700px")
    ]


recipeStyle : Html.Attribute
recipeStyle =
  Attr.style
    [ ("width", "100%")
    , ("text-align", "left")
    , ("margin-bottom", "15px")
    ]

buttonStyle =
  Attr.style
    [ ("background", "#f9a95e")
    , ("margin-right", "3px")
    , ("border-radius", "6px")
    , ("font-size", "16px")
    ]

currentRecipeStyle : Html.Attribute
currentRecipeStyle =
  Attr.style
    [ 
    ]

recipeStorageStyle : Html.Attribute
recipeStorageStyle =
  Attr.style
    [ 
    ]

topStyle : Html.Attribute
topStyle =
  Attr.style
    [ ("margin-bottom", "10px")
    , ("width", "100%")
    ]

recipeContainerStyle =
  Attr.style
    [
    ]