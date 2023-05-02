-- html.hs

module Html
  ( Html,
    Title,
    Structure,
    html_,
    p_,
    h1_,
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

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

p' :: String -> Structure
p' = Structure . el "p"

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
