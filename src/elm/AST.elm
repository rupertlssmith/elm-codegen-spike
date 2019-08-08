module AST exposing (AST, Block(..), example, pretty, prettyToList)

import Pretty exposing (Doc)
import Random exposing (Seed)


type alias AST =
    List Block


type Block
    = Statement (List String)
    | Blocks (List Block)
    | SubBlock Block


pretty : AST -> String
pretty ast =
    prettyAst ast
        |> Pretty.pretty 120


prettyToList : AST -> List String
prettyToList _ =
    [ "prettyToList" ]


example : Int -> Seed -> ( AST, Seed )
example count seed =
    ( [ Statement [ "one", "two", "three" ] ], seed )


prettyAst : AST -> Doc
prettyAst ast =
    List.map prettyBlock ast
        |> Pretty.lines


prettyBlock : Block -> Doc
prettyBlock block =
    case block of
        Statement expr ->
            List.map Pretty.string expr
                |> Pretty.softlines

        Blocks blocks ->
            prettyAst blocks

        SubBlock subBlock ->
            prettyBlock subBlock
                |> Pretty.hang 4
