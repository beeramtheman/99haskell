import Prelude
import FFI

data Ace
data Element
data Event
data Ajax
data Param
data Url
data AjaxRes

-- General

query :: String -> Fay Element
query = ffi "document.querySelector(%1)"

addEvent :: String -> (Event -> Fay()) -> Element -> Fay ()
addEvent = ffi "%3.addEventListener(%1, %2)"

fullscreen :: Element -> Fay ()
fullscreen = ffi "%1.mozRequestFullScreen()"

-- AJAX

makeParam :: [(String, String)] -> Fay Param
makeParam = ffi "(function() { \
    \ var px = []; \
    \ %1.forEach(function(p) { \
    \     px.push(encodeURIComponent(p[0]) + '=' + encodeURIComponent(p[1])); \
    \ }); \
    \ return px.join('&'); \
\ })()"

makeUrl :: String -> Param -> Fay Url
makeUrl = ffi "(function() { return %1 + '?' + %2 })()"

newAjax :: Fay Ajax
newAjax = ffi "new XMLHttpRequest()"

ajaxOpen :: Ajax -> String -> Url -> Bool -> Fay ()
ajaxOpen = ffi "%1.open(%2, %3, %4)"

ajaxHeader :: Ajax -> String -> String -> Fay ()
ajaxHeader = ffi "%1.setRequestHeader(%2, %3)"

ajaxCallback :: Ajax -> (AjaxRes -> Fay ()) -> Fay ()
ajaxCallback = ffi "(function() { \
    \ %1.onreadystatechange = function() { \
    \     if (%1.readyState != 4 || %1.status != 200) return; \
    \     (%2)(%1.responseText); \
    \ }; \
\ })()"

ajaxSend :: Ajax -> Param -> Fay ()
ajaxSend = ffi "%1.send(%2)"

ajax :: String -> String -> [(String, String)] -> (AjaxRes -> Fay()) -> Fay ()
ajax m u p cb
    | m == "get"  = do
        a <- newAjax
        param <- makeParam p
        url <- makeUrl u param
        ajaxCallback a cb
        ajaxOpen a m url False
        ajaxSend a =<< makeParam []
    | m == "post" = do
        a <- newAjax
        url <- makeUrl u =<< makeParam []
        param <- makeParam p
        ajaxCallback a cb
        ajaxOpen a m url False
        ajaxHeader a "Content-type" "application/x-www-form-urlencoded"
        ajaxSend a param

-- Ace editor

ace :: String -> Fay Ace
ace = ffi "ace.edit(%1)"

aceSet :: Ace -> String -> String -> Fay ()
aceSet = ffi "%1.setOption(%2, %3)"

aceSetBool :: Ace -> String -> Bool -> Fay ()
aceSetBool = ffi "%1.setOption(%2, %3)"

aceValue :: Ace -> String
aceValue = ffi "%1.getSession().getValue()"

-- Config

setupTerm :: Fay ()
setupTerm = do
    term <- ace "terminal"
    aceSet term "mode" "ace/mode/haskell"
    aceSet term "theme" "ace/theme/tomorrow"
    aceSetBool term "wrap" True
    aceSetBool term "showGutter" False

setupEvents :: Fay ()
setupEvents = do
    query ".termbar .run" >>= addEvent "click" runCode
    query ".termbar .fullscreen" >>= addEvent "click" fullscreenEditor

-- Events

runCode :: Event -> Fay ()
runCode e = do
    term <- ace "terminal"
    ajax "post" "http://localhost:3000/sandbox" [("code", (aceValue term))] testCallback

fullscreenEditor :: Event -> Fay ()
fullscreenEditor e = fullscreen =<< query "#terminal"

-- Ajax Callbacks

testCallback :: AjaxRes -> Fay ()
testCallback s = print s

-- Main

main :: Fay ()
main = do
    setupTerm
    setupEvents
