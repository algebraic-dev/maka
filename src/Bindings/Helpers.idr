module Bindings.Helpers

public export 
js : String -> String 
js fun = "javascript:lambda:\{fun}"

public export
%foreign (js "(_, x) => x === null || x === undefined ? 1 : 0")
isNullOrUndefined : a -> Bool