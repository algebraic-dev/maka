module Bindings.Dom.Render

import Bindings.Dom.Elements
import Bindings.Helpers
import Virtual.Dom
import Virtual.Diff

import Data.String
import Data.List
import Debug.Trace

mutual 
    renderAttr : Symbol msg model -> NodeElement -> Attribute msg -> IO ()
    renderAttr s parent (OnClick msg) = addEventListener (believe_me parent) s "onclick" msg
    renderAttr s parent (Id id) = setAttribute parent "id" id
    renderAttr s parent (ClassList list) = setAttribute parent "className" (unwords list)

    renderNode : Symbol msg model -> Node msg -> IO NodeElement
    renderNode s (MkNode tag attrs children) = do 
        parent <- createNode tag 
        traverse_ (\a => render s a >>= appendChild parent) children
        traverse_ (renderAttr s parent) attrs
        pure parent

    renderText : String -> IO TextElement
    renderText = createTextNode 

    public export
    render : Symbol msg model -> Html msg -> IO GenericNode
    render s (HtmlNode node) = cast <$> (renderNode s node)
    render s (HtmlText text) = cast <$> (renderText text)

mutual 
    patchAttrs : Symbol msg model -> NodeElement -> List (AttributePatch msg) -> IO ()
    patchAttrs _ el [] = pure ()
    patchAttrs s el (AddAttr attr :: attrs) = renderAttr s el attr >> patchAttrs s el attrs
    patchAttrs s el (RemoveAttr attr :: attrs) = removeAttr el attr >> patchAttrs s el attrs
        where 
            removeAttr : NodeElement -> Attribute msg -> IO ()
            removeAttr el (OnClick msg) = removeListener el "onevent"
            removeAttr el (Id _)        = removeAttribute el "id"
            removeAttr el (ClassList _) = removeAttribute el "className"

    patchChildren : Symbol msg model -> GenericNode -> List GenericNode -> List (Patch msg) -> IO ()
    patchChildren s par (x :: xs) (p :: ps) = patch s x p   >> patchChildren s par xs ps 
    patchChildren s par []        (p :: ps) = patch s par p >> patchChildren s par [] ps
    patchChildren s _   []               [] = pure ()

    public export
    patch : Symbol msg model -> GenericNode -> Patch msg -> IO ()
    patch _ el NoPatch                 = pure ()
    patch s el (Replace html)          = render s html >>= replaceWith (cast el)
    
    patch s el (Update children attrs) = do 
        realChildren <- getChildren (believe_me el)
        patchChildren s el realChildren children 
        patchAttrs s (believe_me el) attrs 

    patch s el (Add x) = do 
        res <- render s x 
        appendChild (believe_me el) res

    patch s el Remove = remove el