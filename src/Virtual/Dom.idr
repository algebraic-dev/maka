module Virtual.Dom

import Generics.Derive
%language ElabReflection

public export
data Attribute msg 
    = Id String 
    | ClassList (List String) 
    | OnClick msg

%runElab derive "Attribute" [Generic, Eq]

mutual 
    public export
    record Node (msg : Type) where
        constructor MkNode
        tag        : String 
        attributes : List (Attribute msg) 
        children   : List (Html msg)

    public export
    data Html msg = HtmlNode (Node msg) | HtmlText String