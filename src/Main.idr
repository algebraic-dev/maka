module Main 

import Maka.Elements
import Maka.Attributes
import Maka.App

import Generics.Derive

%language ElabReflection

data Model = Increment | Decrement

Eq Model where 
  (==) Increment Increment = True
  (==) Decrement Decrement = True
  (==) _ _ = False 

update : Model -> Int -> Int 
update Increment = (+ 1)
update Decrement = (+ (- 1))

view : Int -> Html Model 
view counter =
    div [id "main"] 
      [ button [onClick Increment] [text "Incrementar"]
      , p [] [text "Contador: \{show counter}"]
      , if counter `mod` 3 == 1 then button [onClick Decrement] [text "Decrementar"] else (text "Nada")
      ] 

main : IO ()
main = do 
  start view update 0