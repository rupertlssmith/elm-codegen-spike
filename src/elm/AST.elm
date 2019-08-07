module AST exposing (AST, Block(..))

import Pretty


type alias AST =
    List Block


type Block
    = Statement (List String)
    | Blocks (List Block)
    | SubBlock Block
