module Main 

import Maka.Elements
import Maka.Attributes
import Maka.App

data Model = Increment | Decrement

Eq Model where 
  (==) Increment Increment = True
  (==) Decrement Decrement = True
  (==) _ _ = False

update : Model -> Int -> Int 
update msg counter =
  case msg of 
    Increment => counter + 1
    Decrement => counter - 1

view : Int -> Html Model 
view counter =
    div [id "main"] 
      [ button [onClick Increment] [text "Incrementar"]
      , p [] [text "Contador: \{show counter}"]
      , button [onClick Decrement] [text "Decrementar"]
      ]

main : IO ()
main = start view update 0