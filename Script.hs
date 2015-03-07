import Prelude hiding (lookup)
import FFI

data Ace
data Element
data Event
data Ajax
data Param
data Url
data AjaxRes
data Html
data Object

-- General

query :: String -> Fay Element
query = ffi "document.querySelector(%1)"

addEvent :: String -> (Event -> Fay ()) -> Element -> Fay ()
addEvent = ffi "%3.addEventListener(%1, %2)"

fullscreen :: Element -> Fay ()
fullscreen = ffi "%1.mozRequestFullScreen()"

getData :: String -> Element -> Fay String
getData = ffi "%2['dataset'][%1]"

fromJson :: a -> Fay a
fromJson = ffi "JSON.parse(%1)"

setHtml :: Html -> Element -> Fay ()
setHtml = ffi "%2['innerHTML'] = %1"

showBlock :: Element -> Fay ()
showBlock = ffi "%1['style']['display'] = 'block'"

lookup :: String -> a -> Fay b
lookup = ffi "%2[%1]"

forEach :: (a -> Fay ()) -> a -> Fay ()
forEach = ffi "%2.forEach(%1)"

makeObject :: Fay Object
makeObject = ffi "{}"

objAddStr :: String -> String -> Object -> Fay Object
objAddStr = ffi "(function() { %3[%1] = %2; return %3; })()"

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

aceValue :: Ace -> Fay String
aceValue = ffi "%1.getSession().getValue()"

addCommand :: Ace -> String -> Object -> (Event -> Fay ()) -> Fay ()
addCommand = ffi "%1.commands.addCommand({ \
    \ name: %2, \
    \ bindKey: %3, \
    \ exec: %4 \
\ })"

-- Config

setupTerm :: Fay ()
setupTerm = do
    term <- ace "terminal"
    aceSet term "mode" "ace/mode/haskell"
    aceSet term "theme" "ace/theme/tomorrow"
    aceSetBool term "wrap" True
    aceSetBool term "showGutter" False

    keys <- objAddStr "win" "Ctrl-Enter"
            =<< objAddStr "mac" "Command-Enter"
            =<< makeObject

    addCommand term "test" keys runCode

setupEvents :: Fay ()
setupEvents = do
    query ".termbar .run" >>= addEvent "click" runCode
    query ".termbar .fullscreen" >>= addEvent "click" fullscreenEditor

setupFunctions :: Fay ()
setupFunctions = do
    ffi "window.escape = function(h) { return h.replace(/&/g, '&amp;').replace \
        \ (/</g, '&lt;').replace(/\\n/g, '<br>').replace(/ /g, '&nbsp;') }"

-- Events

runCode :: Event -> Fay ()
runCode e = do
    term <- ace "terminal"
    datax <- query ".datax"
    num <- getData "num" datax
    aceVal <- aceValue term
    ajax "post"
         ("http://localhost:3000/sandbox/" ++ num)
         [("code", aceVal)]
         evaluateMark

fullscreenEditor :: Event -> Fay ()
fullscreenEditor e = fullscreen =<< query "#terminal"

-- Ajax Callbacks

evaluateMark :: AjaxRes -> Fay ()
evaluateMark m = do
    out <- query "section.out"
    res <- fromJson m
    markHtml <- makeMarkHtml =<< lookup "tests" res
    setHtml markHtml out
    success <- lookup "success" res
    if success then showBlock =<< query "nav" else return ()

-- HTML

makeMarkHtml :: a -> Fay Html
makeMarkHtml = ffi "(function() { \
    \ var html = ''; \
    \ %1.forEach(function(t, i) { \
    \     html += '<div class=\"test\">\\\
    \                 <div class=\"success ' + t.success + '\"></div>\\\
    \                 <div class=\"overview\">' + escape(t.test[0]) + '\\\
    \                 -> ' + escape(t.test[1]); \
    \     if(!t.success) { \
    \         html += '<br>' + escape(t.output); \
    \     } \
    \     html += '</div></div>'; \
    \ }); \
    \ return html; \
\ })()"

-- Main

main :: Fay ()
main = do
    setupFunctions
    setupTerm
    setupEvents
