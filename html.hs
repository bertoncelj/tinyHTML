-- html.hs

module Html
  ( Html,
    Title,
    Structure,
    html_,
    p_,
    h1_,
    ul_,
    ol_,
    code_,
    printStruct,
    append_,
    render,
  )
where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

el :: String -> String -> String
el = \tag -> \content ->
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

getStructure :: Structure -> String
getStructure struct =
  case struct of
    Structure str -> str

dot_ :: (b -> c) -> (a -> b) -> a -> c
dot_ f g x = f (g x)

append_ :: Structure -> Structure -> Structure
append_ s1 s2 = Structure (getStructureString s1 <> getStructureString s2)

render :: Html -> String
render html =
  case html of
    Html str -> str

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concat . map escapeChar

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ul" . concat . map (el "ol" . getStructureString)

printStruct (Structure s) = s

-- haihi = [p_ "neki", p_ "jaja", p_ "tine"]
