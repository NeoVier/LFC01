module Models.Regex exposing (..)

import Models.Alphabet as Alphabet


type Regex
    = Epsilon
    | Symbol Alphabet.Symbol
    | Union Regex Regex
    | Concat Regex Regex
    | Star Regex
    | Plus Regex
    | Question Regex
    | Group (List GroupInner)


type alias GroupInner =
    ( Alphabet.Symbol, Alphabet.Symbol )


type alias IdRegex =
    ( String, Regex )


groupInnerToString : GroupInner -> String
groupInnerToString x =
    case x of
        ( x1, x2 ) ->
            String.fromList <| [ x1, '-', x2 ]


toString : Regex -> String
toString r =
    case r of
        Epsilon ->
            "&"

        Symbol s ->
            String.fromChar s

        Concat r1 r2 ->
            "(" ++ toString r1 ++ toString r2 ++ ")"

        Union r1 r2 ->
            "{" ++ toString r1 ++ " | " ++ toString r2 ++ "}"

        Star r1 ->
            toString r1 ++ "*"

        Plus r1 ->
            toString r1 ++ "+"

        Question r1 ->
            toString r1 ++ "?"

        Group ss ->
            "[" ++ (List.map groupInnerToString ss |> String.join "") ++ "]"
