module Bindings.Structure.Array

import Bindings.Helpers

||| Opaque type to represent an array in javascript
||| 
|||   @ty The type of each element of the array
|||
data Array : (ty: Type) -> Type where 

%foreign (js "(_, array, idx) => array[idx]")
unsafeIndex  : (1 _ : Array a) -> Nat -> a

indexArr : Array a -> Nat -> Maybe (a, Array a) 
indexArr arr n =
    let res = unsafeIndex arr n in 
    if isNullOrUndefined res 
        then Nothing 
        else Just (res, arr)

public export
idxArr : (1 _ : Array a) -> Nat -> Maybe (a, Array a)
idxArr arr s = assert_linear indexArr arr s
