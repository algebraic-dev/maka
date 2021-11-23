module Main 

import Maka.Elements
import Maka.Attributes
import Bindings.Dom.Elements
import Bindings.Dom.Render
import Virtual.Diff

data Model = Increment | Decrement

Eq Model where 
  (==) Increment Increment = True
  (==) Decrement Decrement = True
  (==) _ _ = False

update : Model -> Int -> Int 
update msg counter =
  case msg of 
    Increment => counter + 1
    Decrement => counter + 1

view : Int -> Html Model 
view counter =
    div [id "main"] 
      [ button [onClick Increment] [text "Incrementar"]
      , p [] [text "Contador: \{show counter}"]
      , button [onClick Decrement] [text "Decrementar"]
      ]

main : IO ()
main = do 
  Just main <- getElementById "main" 
    | Nothing => putStrLn "Não tem o main"

  rendered <- render (view 100)
  appendChild (cast main) rendered

  let d = diff (view 1) (view 2)

  patch (believe_me rendered) d

  putStrLn "Rendered"