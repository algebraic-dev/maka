module Bindings.Dom.Elements

import Bindings.Helpers

-- Opaque types that will represent the data 
-- in javascript 

public export data GenericNode : Type    where 
public export data NodeElement : Type    where 
public export data TextElement : Type    where 
public export data Children    : Type    where
public export data Symbol : Type -> Type -> Type where

public export
data Element = NodeEl NodeElement | TextEl TextElement

%foreign (js "(tag) => document.createElement(tag)")
prim__createNode : String -> PrimIO NodeElement

%foreign (js "(text) => document.createTextNode(text)")
prim__createTextNode  : String -> PrimIO TextElement

%foreign (js "(el, attr, val) => el.setAttribute(attr, val)")
prim__setAttribute : NodeElement -> String -> String -> PrimIO ()

%foreign (js "(parent, child) => parent.appendChild(child)")
prim__appendChild : NodeElement -> GenericNode -> PrimIO ()

%foreign (js "(child) => child.parentNode")
prim__getParent : GenericNode -> PrimIO NodeElement

%foreign (js "(id) => el.remove()")
prim__remove : GenericNode -> PrimIO ()

%foreign (js "(old, neww) => old.replaceWith(neww)")
prim__replaceWith : GenericNode -> GenericNode -> PrimIO ()

%foreign (js """
             (_, _1, el, sym, ev, data) => {
                 el.addEventListener(ev, () => {
                    if(window.events && window.events.get(sym)) {
                        let [fn, state] = window.events.get(sym);
                        window.events.set(sym, [fn, fn(data, state)])
                    }
                 })
             }
             """)
prim__addEventListener : GenericNode -> Symbol msg model -> String -> msg -> PrimIO ()

%foreign (js "(el, attr) => el.removeAttribute(attr)")
prim__removeAttribute : NodeElement -> String -> PrimIO ()

%foreign (js "(id) => document.getElementById(id)")
prim__getElementById  : String -> PrimIO NodeElement

%foreign (js "(el) => el.childNodes")
prim__getChildren : NodeElement -> PrimIO Children 

%foreign (js "(el) => el.length")
prim__childLength : Children -> PrimIO Int 

%foreign (js "(child, n) => child[n]")
prim__idxChild : Children -> Int -> PrimIO GenericNode

%foreign (js "(_, _1, name) => Symbol(name)")
prim__createSymbol : String -> Symbol a b

%foreign(js """
            (_, _1, _2, symbol, fn, def, world) => {
                if(window.events == undefined) {
                    window.events = new Map();
                }
                window.events.set(symbol, [(a,b) => fn(a)(b)(world), def])
            }
            """)
prim__addListener : Symbol a b -> (a -> (b,c) -> IO (b, c)) -> (b, c) -> PrimIO ()

public export
Cast Element GenericNode where 
    cast (NodeEl n) = believe_me n
    cast (TextEl n) = believe_me n

public export Cast NodeElement GenericNode where cast n = believe_me n
public export Cast TextElement GenericNode where cast n = believe_me n

public export
addEventListener : GenericNode -> Symbol a b -> String -> a -> IO ()
addEventListener g s ev ret = primIO $ prim__addEventListener g s ev ret 


public export 
createSymbol : String -> Symbol a b
createSymbol = prim__createSymbol

public export 
addListener : Symbol a b -> (a -> (b,c) -> IO (b,c)) -> (b,c) -> IO ()
addListener symbol fn init = primIO $ prim__addListener symbol fn init

public export
getParent : GenericNode -> IO NodeElement
getParent = primIO . prim__getParent

public export
createNode : String -> IO NodeElement
createNode = primIO . prim__createNode

public export
createTextNode : String -> IO TextElement
createTextNode str = primIO $ prim__createTextNode str

public export
setAttribute : NodeElement -> String -> String -> IO ()
setAttribute el attr val = primIO $ prim__setAttribute el attr val 

public export
appendChild : NodeElement -> GenericNode -> IO ()
appendChild el gn = primIO $ prim__appendChild el gn

public export
remove : GenericNode -> IO ()
remove el = primIO $ prim__remove el

public export
replaceWith : GenericNode -> GenericNode -> IO ()
replaceWith g g' = primIO $ prim__replaceWith g g'

public export
removeAttribute : NodeElement -> String -> IO ()
removeAttribute el str = primIO $ prim__removeAttribute el str

public export 
getElementById : String -> IO (Maybe NodeElement)
getElementById str = do 
    res <- primIO $ prim__getElementById str
    if isNullOrUndefined res 
        then pure $ Nothing 
        else pure $ Just res

public export 
getChildren : NodeElement -> IO (List GenericNode)
getChildren el = do 
    c <- primIO $ prim__getChildren el
    l <- primIO $ prim__childLength c
    if l <= 0 
        then pure [] 
        else for [0..(l-1)] (\n => primIO $ prim__idxChild c n)