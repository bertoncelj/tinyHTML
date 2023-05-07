import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
        (h1_ "Heading")
        ( append_
            (p_ "Paragraph1")
            (p_ "Paragraph2")
        )
    )

haihi = [p_ "neki", p_ "jaja", p_ "tine"]

test_new_ul = ul_ haihi
