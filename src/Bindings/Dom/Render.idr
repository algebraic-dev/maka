module Bindings.Dom.Render

import Bindings.Dom.Elements
import Bindings.Helpers
import Virtual.Dom
import Virtual.Diff

import Data.String
import Data.List

mutual 
    renderAttr : NodeElement -> Attribute msg -> IO ()
    renderAttr parent (OnClick msg) = pure () -- To implement
    renderAttr parent (Id id) = setAttribute parent "id" id
    renderAttr parent (ClassList list) = setAttribute parent "className" (unwords list)

    renderNode : Node msg -> IO NodeElement
    renderNode (MkNode tag attrs children) = do 
        parent <- createNode tag 
        traverse_ (\a => render a >>= appendChild parent) children
        traverse_ (renderAttr parent) attrs
        pure parent

    renderText : String -> IO TextElement
    renderText = createTextNode 

    public export
    render : Html msg -> IO GenericNode
    render (HtmlNode node) = cast <$> (renderNode node)
    render (HtmlText text) = cast <$> (renderText text)

mutual 
    patchAttrs : NodeElement -> List (AttributePatch msg) -> IO ()
    patchAttrs el patch = pure ()

    patchChildren : List GenericNode -> List (Patch msg) -> IO ()
    patchChildren nodes patches  = for_ (zip nodes patches) (uncurry patch)

    public export
    patch : GenericNode -> Patch msg -> IO ()
    patch el NoPatch                 = pure ()
    patch el (Replace html)          = render html >>= replaceWith (cast el)
    
    patch el (Update children attrs) = do 
        realChildren <- getChildren (believe_me el)
        patchChildren realChildren children 
        patchAttrs (believe_me el) attrs 

    patch el (Add x) = ?e
    patch el Remove  = ?c