module Main 

import Maka.Elements
import Maka.Attributes
import Maka.App

import Generics.Derive

%language ElabReflection

data Model = Increment | Decrement | Add Int

%runElab derive "Model" [Generic, Eq]

Show Model where 
  show Increment = "inc"
  show Decrement = "dec"
  show (Add n) = "add \{show n}"

update : Model -> Int -> Int 
update Increment = (+ 1)
update Decrement = (+ (- 1))
update (Add n)   = (+ n)

view : Int -> Html Model 
view counter =
    div [id "main"] 
      [ button [onClick Increment] [text "Incrementar"]
      , p [] [text "Contador: \{show counter}"]
      , if counter `mod` 2 == 1 then button [onClick Decrement] [text "Decrementar"] else (text "Nada")
      ] 

main : IO ()
main = do 
  start view update 0