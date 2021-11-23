module Virtual.Diff 

import Virtual.Dom
import Data.List
import Debug.Trace

public export
data AttributePatch msg 
    = AddAttr (Attribute msg)
    | RemoveAttr (Attribute msg)

public export
data Patch msg 
    = Add (Html msg)
    | Remove 
    | Update (List (Patch msg)) (List (AttributePatch msg))
    | Replace (Html msg)
    | NoPatch

public export
Show (AttributePatch msg) where 
    show (AddAttr s) = "+attr"
    show (RemoveAttr s) = "-attr"

public export
Show (Patch msg) where 
    show (Add n) = "+"
    show (Remove) = "-"
    show (Update n l) = "(update \{show n} \{show l})"
    show (Replace n) = "replace"
    show NoPatch = "∅"

mutual
    diffChildren : Eq msg => List (Html msg) -> List (Html msg) -> List (Patch msg) -> List (Patch msg)
    diffChildren []        []        acc = acc
    diffChildren (x :: xs) []        acc = diffChildren xs [] (Remove   :: acc)
    diffChildren [] (y :: ys)        acc = diffChildren [] ys (Add y    :: acc)
    diffChildren (x :: xs) (y :: ys) acc = diffChildren xs ys (diff x y :: acc)

    diffAttrs : Eq msg => List (Attribute msg) -> List (Attribute msg) -> List (AttributePatch msg) 
    diffAttrs old new = 
        let removed = old \\ new
            added   = new \\ old 
        in (map RemoveAttr removed) 
        ++ (map AddAttr added)
    
    public export
    diff : Eq msg => Html msg -> Html msg -> Patch msg 
    diff (HtmlNode node) new@(HtmlText text)  = Replace new
    diff (HtmlText text) new@(HtmlNode node)  = Replace new
    diff (HtmlText text) new@(HtmlText text') with (text == text')
        _ | True  = NoPatch
        _ | False = Replace new
    diff (HtmlNode node) new@(HtmlNode node') with (node.tag /= node'.tag)
        _ | True  = Replace new 
        _ | False = let children = (diffChildren `on` children) node node' []
                        attrs    = (diffAttrs `on` attributes)  node node'
                    in Update (reverse children) attrs