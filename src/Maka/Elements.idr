module Maka.Elements

import public Virtual.Dom

public export
ElementNode : Type -> Type
ElementNode msg = List (Attribute msg) -> List (Html msg) -> Html msg

public export
mk : String -> ElementNode msg
mk tag = HtmlNode .: MkNode tag

public export
text : String -> Html msg 
text = HtmlText

public export
div : ElementNode msg 
div = mk "div"

public export
button : ElementNode msg 
button = mk "button"

public export
p : ElementNode msg 
p = mk "p"
