module AST exposing (AST, Block(..), example, pretty, prettyToList)

import Pretty


type alias AST =
    List Block


type Block
    = Statement (List String)
    | Blocks (List Block)
    | SubBlock Block


pretty : AST -> String
pretty _ =
    "pretty"


prettyToList : AST -> List String
prettyToList _ =
    [ "prettyToList" ]


example : Int -> AST
example _ =
    []
