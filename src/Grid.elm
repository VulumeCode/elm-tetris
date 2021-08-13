module Grid exposing
    ( Grid
    , coordinates
    , dimensions
    , down
    , empty
    , filter
    , fromKey
    , get
    , insert
    , left
    , map
    , member
    , positions
    , remove
    , right
    , toList
    , up
    )

import Dict exposing (Dict)


type Grid a
    = Grid (Internal a)


type alias Internal a =
    { x : Int, y : Int, cells : Dict String a }


empty : ( Int, Int ) -> Grid a
empty ( x, y ) =
    Grid { x = x, y = y, cells = Dict.empty }


left : ( Int, Int ) -> ( Int, Int )
left ( x, y ) =
    ( x, y + 1 )


right : ( Int, Int ) -> ( Int, Int )
right ( x, y ) =
    ( x + 1, y )


down : ( Int, Int ) -> ( Int, Int )
down ( x, y ) =
    ( x + 1, y + 1 )


up : ( Int, Int ) -> ( Int, Int )
up ( x, y ) =
    ( x - 1, y - 1 )


dimensions : Grid a -> ( Int, Int )
dimensions (Grid { x, y }) =
    ( x, y )


coordinates : Grid a -> List ( Int, Int )
coordinates (Grid { x, y }) =
    List.range 0 ((x * y) - 1)
        |> List.map (\cellNumber -> ( modBy x cellNumber, cellNumber // x ))
        |> List.filter (\(x1,y1) -> (abs x1 - y1) < 10 && (abs y1 - x1) < 10)


key : ( Int, Int ) -> String
key ( x, y ) =
    [ x, y ]
        |> List.map String.fromInt
        |> String.join ","


fromKey : String -> ( Int, Int )
fromKey k =
    case List.map (Maybe.withDefault 0 << String.toInt) (String.split "," k) of
        [ x, y ] ->
            ( x, y )

        _ ->
            ( 1000, 1000 )


get : ( Int, Int ) -> Grid v -> Maybe v
get position (Grid grid) =
    Dict.get (key position) grid.cells


filter : (( Int, Int ) -> v -> Bool) -> Grid v -> Grid v
filter f (Grid grid) =
    Grid { grid | cells = Dict.filter (\k v -> f (fromKey k) v) grid.cells }


insert : ( Int, Int ) -> v -> Grid v -> Grid v
insert position value (Grid grid) =
    Grid { grid | cells = Dict.insert (key position) value grid.cells }


remove : ( Int, Int ) -> Grid v -> Grid v
remove position (Grid grid) =
    Grid { grid | cells = Dict.remove (key position) grid.cells }


member : ( Int, Int ) -> Grid a -> Bool
member position (Grid grid) =
    Dict.member (key position) grid.cells


map : (( Int, Int ) -> a -> b) -> Grid a -> Grid b
map f (Grid grid) =
    Grid { x = grid.x, y = grid.y, cells = Dict.map (\k v -> f (fromKey k) v) grid.cells }


positions : Grid a -> List ( Int, Int )
positions (Grid grid) =
    grid.cells
        |> Dict.keys
        |> List.map fromKey


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid grid) =
    grid.cells
        |> Dict.toList
        |> List.map (Tuple.mapFirst fromKey)
