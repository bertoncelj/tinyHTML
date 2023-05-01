el :: String -> String -> String
el = \tag -> \content ->
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

wrapHtml :: String -> String
wrapHtml content = "<html><body>" ++ content ++ "</body></html>"

html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"

p_ :: String -> String
p_ = el "p"

h1_ :: String -> String
h1_ = el "h1"

head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

title_ :: String -> String
title_ content = "<title>" <> content <> "</title>"

makeHtml :: String -> String -> String
makeHtml t c = html_ ((head_ (title_ t)) <> (body_ c))

richerHtml t c para = html_ ((head_ (title_ (h1_ t))) <> (body_ (c) <> (p_ para)))

myhtml :: String
-- myhtml = makeHtml "Hello World Title" "Neki Neki content!"
myhtml = richerHtml "Hello World Title" "Neki Neki content!" "In a paragraph"

add_ = \a -> \b -> a + b

newtype Html = Html String

newtype Structure = Structure String

p' :: String -> Structure
p' = Structure . el "p"

getStructure :: Structure -> String
getStructure struct =
  case struct of
    Structure str -> str

main :: IO ()
main = putStrLn myhtml
