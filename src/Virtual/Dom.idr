module Virtual.Dom

public export
data Attribute msg 
    = Id String 
    | ClassList (List String) 
    | OnClick msg

public export 
Eq msg => Eq (Attribute msg) where 
    (==) (Id str) (Id str')             = str == str'
    (==) (ClassList ls) (ClassList ls') = ls == ls'
    (==) (OnClick msg) (OnClick msg')   = msg == msg' 
    (==) _                            _ = False

mutual 
    public export
    record Node (msg : Type) where
        constructor MkNode
        tag        : String 
        attributes : List (Attribute msg) 
        children   : List (Html msg)

    public export
    data Html msg = HtmlNode (Node msg) | HtmlText String