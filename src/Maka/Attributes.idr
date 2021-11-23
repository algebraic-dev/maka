module Maka.Attributes

import Virtual.Dom

public export
onClick : msg -> Attribute msg 
onClick = OnClick 

public export
id : String -> Attribute msg 
id = Id