module Tetrimino exposing (Tetrimino(..), color, random, rotateAround, spawn)

import Random


type Tetrimino
    = I
    | O
    | T
    | S
    | Z
    | J
    | L


random : Random.Generator Tetrimino
random =
    Random.uniform I [ O, T, S, Z, J, L ]


color : Tetrimino -> String
color tetrimino =
    case tetrimino of
        I ->
            "text-teal-400"

        O ->
            "text-yellow-400"

        T ->
            "text-pink-400"

        S ->
            "text-green-400"

        Z ->
            "text-red-400"

        J ->
            "text-blue-400"

        L ->
            "text-orange-400"


spawn : Tetrimino -> List ( ( Int, Int ), ( Tetrimino, Bool ) )
spawn tetrimino =
    case tetrimino of
        I ->
            [ ( ( 0, 0 ), ( I, False ) )
            , ( ( 1, 0 ), ( I, True ) )
            , ( ( 2, 0 ), ( I, False ) )
            , ( ( 3, 0 ), ( I, False ) )
            ]

        O ->
            [ ( ( 1, 0 ), ( O, False ) )
            , ( ( 0, 0 ), ( O, False ) )
            , ( ( 1, 1 ), ( O, False ) )
            , ( ( 0, 1 ), ( O, False ) )
            ]

        T ->
            [ ( ( 0, 0 ), ( T, False ) )
            , ( ( 1, 0 ), ( T, True ) )
            , ( ( 1, 1 ), ( T, False ) )
            , ( ( 2, 0 ), ( T, False ) )
            ]

        S ->
            [ ( ( 1, 0 ), ( S, False ) )
            , ( ( 2, 0 ), ( S, False ) )
            , ( ( 0, 1 ), ( S, False ) )
            , ( ( 1, 1 ), ( S, True ) )
            ]

        Z ->
            [ ( ( 0, 0 ), ( Z, False ) )
            , ( ( 1, 0 ), ( Z, False ) )
            , ( ( 1, 1 ), ( Z, True ) )
            , ( ( 2, 1 ), ( Z, False ) )
            ]

        J ->
            [ ( ( 0, 0 ), ( J, False ) )
            , ( ( 1, 0 ), ( J, True ) )
            , ( ( 2, 0 ), ( J, False ) )
            , ( ( 2, 1 ), ( J, False ) )
            ]

        L ->
            [ ( ( 0, 0 ), ( L, False ) )
            , ( ( 1, 0 ), ( L, True ) )
            , ( ( 2, 0 ), ( L, False ) )
            , ( ( 0, 1 ), ( L, False ) )
            ]


rotateAround : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
rotateAround ( x, y ) ( cx, cy ) =
    ( ((y - cy) * -1) + cx, (x - cx) + cy )
