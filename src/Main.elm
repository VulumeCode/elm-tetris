module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Cell exposing (Cell)
import Dict
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Value)
import Process
import Random
import Storage exposing (Storage)
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Tetrimino exposing (Tetrimino)
import Time
import Set


type alias Model =
    { gridState : Grid Cell
    , storage : Maybe Storage
    , state : State
    , lines : Int
    , mode : Mode
    , control : Maybe Control
    , clearing : Set.Set (Int,Int)
    }


type State
    = Playing
    | Settling
    | GameOver


type Foul
    = OutOfBounds
    | CellTaken


type Mode
    = Normal
    | Medium
    | Hard


type Control
    = Left
    | Right
    | Down


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { gridState = Grid.empty ( 20, 20 )
      , storage =
            Decode.decodeValue (Decode.field "storage" Decode.string) flags
                |> Result.andThen (Decode.decodeString Storage.decoder)
                |> Result.toMaybe
      , state = Playing
      , lines = 0
      , mode = Normal
      , control = Nothing
      , clearing = Set.empty
      }
    , Random.generate Spawn Tetrimino.random
    )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Slantris"
    , body = [ viewRoot model ]
    }


viewRoot : Model -> Html Msg
viewRoot model =
    Html.div [ Attributes.class "flex flex-col fixed inset-0 h-full items-center select-none font-mono" ]
        [ viewBoard model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Html.div [ Attributes.class "flex flex-col h-full w-full max-h-screen sm:max-w-xl relative" ]
        [ viewHeader model
        , Html.div [ Attributes.class "relative flex flex-1 min-h-0" ]
            [ Svg.svg
                [ Grid.dimensions model.gridState
                    |> (\( x, y ) -> [ 0, 0, x * Cell.size, y * Cell.size ])
                    |> List.map String.fromInt
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , Svg.Attributes.width "100%"
                , Svg.Attributes.height "100%"
                , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                , Svg.Attributes.transform "rotate(45)"
                ]
                (List.map (viewCell model.gridState model.clearing) (Grid.coordinates model.gridState))
            , overlayControls
            ]
        , viewControls
        , viewOverlay model
        ]


viewCell : Grid Cell -> Set.Set (Int, Int) -> ( Int, Int ) -> Svg Msg
viewCell gridState clearing ( x, y )  =
    Svg.rect
        [ Svg.Attributes.class
            (String.join " "
                [ "fill-current"
                , 
                if 
                    Set.member (x,y) clearing
                then
                    "animate-blink text-yellow-500"
                else
                    Grid.get ( x, y ) gridState
                        |> Maybe.map Cell.color
                        |> Maybe.withDefault "text-gray-700"
                ]
            )
        , Svg.Attributes.width (String.fromInt Cell.size)
        , Svg.Attributes.height (String.fromInt Cell.size)
        , Svg.Attributes.strokeWidth (String.fromFloat (0.05 * toFloat Cell.size))
        , Svg.Attributes.stroke "rgb(31, 41, 55)"
        , Svg.Attributes.x (String.fromInt (x * Cell.size))
        , Svg.Attributes.y (String.fromInt (y * Cell.size))
        ]
        []


viewHeader : Model -> Html Msg
viewHeader model =
    let
        rank hs =
            if hs < 10 then
                "👍"

            else if hs < 50 then
                "⭐️"

            else if hs < 100 then
                "🔥"

            else
                "🏆"
    in
    Html.div [ Attributes.class "flex items-center justify-between p-4" ]
        [ Html.span [ Attributes.class "ml-4 text-gray-200" ]
            [ Html.text ("lines - " ++ String.fromInt model.lines)
            , case model.storage of
                Just storage ->
                    Html.span [ Attributes.class "ml-4 text-gray-700 opacity-75" ]
                        [ Html.text <|
                            "("
                                ++ rank (Storage.highScore storage)
                                ++ String.fromInt (Storage.highScore storage)
                                ++ ")"
                        ]

                Nothing ->
                    Html.text ""
            ]
        , Html.div [ Attributes.class "" ]
            [ Html.button
                [ Attributes.classList
                    [ ( "px-2 leading-snug rounded", True )
                    , ( "bg-yellow-400", model.mode == Normal )
                    ]
                , Events.onMouseDown (SelectMode Normal)
                ]
                [ Html.span [ Attributes.class "mt-1 flex flex-col text-3xl" ]
                    [ Html.text "🙂"
                    ]
                ]
            , Html.button
                [ Attributes.classList
                    [ ( "px-2 leading-snug rounded", True )
                    , ( "bg-teal-400", model.mode == Medium )
                    ]
                , Events.onMouseDown (SelectMode Medium)
                ]
                [ Html.span [ Attributes.class "mt-1 flex flex-col text-3xl" ]
                    [ Html.text "😎"
                    ]
                ]
            , Html.button
                [ Attributes.classList
                    [ ( "px-2 leading-snug rounded", True )
                    , ( "bg-red-400", model.mode == Hard )
                    ]
                , Events.onMouseDown (SelectMode Hard)
                ]
                [ Html.span [ Attributes.class "mt-1 flex flex-col text-3xl" ]
                    [ Html.text "\u{1F92F}"
                    ]
                ]
            ]
        ]


viewControls : Html Msg
viewControls =
    Html.div [ Attributes.class "flex h-24", Attributes.style "z-index" "1" ]
        [ Html.button
            [ Attributes.class "h-full w-1/4"
            , Events.on "pointerdown" (Decode.succeed <| SetControl Left)
            , Events.on "pointerup" (Decode.succeed <| CancelControl Left)
            , Events.on "pointerout" (Decode.succeed <| CancelControl Left)
            ]
            [ Html.span [ Attributes.class "flex flex-col text-3xl" ]
                [ Html.text "👈"
                ]
            ]
        , Html.button
            [ Attributes.class "h-full w-1/4"
            , Events.on "pointerdown" (Decode.succeed Rotate)
            ]
            [ Html.span [ Attributes.class "flex flex-col text-3xl" ]
                [ Html.text "🤞"
                ]
            ]
        , Html.button
            [ Attributes.class "h-full w-1/4"
            , Events.on "pointerdown" (Decode.succeed <| SetControl Down)
            , Events.on "pointerup" (Decode.succeed <| CancelControl Down)
            , Events.on "pointerout" (Decode.succeed <| CancelControl Down)
            ]
            [ Html.span [ Attributes.class "flex flex-col text-3xl" ]
                [ Html.text "👇"
                ]
            ]
        , Html.button
            [ Attributes.class "h-full w-1/4"
            , Events.on "pointerdown" (Decode.succeed <| SetControl Right)
            , Events.on "pointerup" (Decode.succeed <| CancelControl Right)
            , Events.on "pointerout" (Decode.succeed <| CancelControl Right)
            ]
            [ Html.span [ Attributes.class "flex flex-col text-3xl" ]
                [ Html.text "👉"
                ]
            ]
        ]


overlayControls : Html Msg
overlayControls =
    Html.div
        [ Attributes.class "absolute inset-0 flex"
        ]
        [ Html.button
            [ Attributes.class "flex flex-1 h-full"
            , Events.on "pointerdown" (Decode.succeed <| SetControl Left)
            , Events.on "pointerup" (Decode.succeed <| CancelControl Left)
            , Events.on "pointerout" (Decode.succeed <| CancelControl Left)
            ]
            []
        , Html.button
            [ Attributes.class "flex flex-1 h-full"
            , Events.on "pointerdown" (Decode.succeed <| SetControl Right)
            , Events.on "pointerup" (Decode.succeed <| CancelControl Right)
            , Events.on "pointerout" (Decode.succeed <| CancelControl Right)
            ]
            []
        ]


viewOverlay : Model -> Html Msg
viewOverlay model =
    let
        isNewBest =
            model.lines > Maybe.withDefault 0 (Maybe.map Storage.highScore model.storage)
    in
    case model.state of
        GameOver ->
            Html.div
                [ Attributes.class "absolute inset-0 flex flex-col justify-center items-center bg-black text-white"
                , Attributes.style "z-index" "2"
                ]
                [ Html.button
                    [ Attributes.class "p-10"
                    , Events.onClick Restart
                    ]
                    [ Html.span [ Attributes.class "flex flex-col" ]
                        [ Html.span [ Attributes.class "text-xl font-medium" ]
                            [ Html.text <|
                                String.join " "
                                    [ if isNewBest then
                                        "New Best - "

                                      else
                                        ""
                                    , String.fromInt model.lines
                                    , if model.lines == 1 then
                                        "Line"

                                      else
                                        "Lines"
                                    ]
                            ]
                        , Html.span [ Attributes.class "text-3xl font-medium" ]
                            [ Html.text "Game Over"
                            ]
                        , Html.span [] [ Html.text "Restart?" ]
                        ]
                    ]
                ]

        _ ->
            Html.text ""



-- UPDATE


type Msg
    = GotStorage (Maybe Storage)
    | SelectMode Mode
    | Spawn Tetrimino
    | SetControl Control
    | CancelControl Control
    | Move Control
    | Rotate
    | Advance
    | Place
    | SettleBoard (Dict.Dict Int (List (Int, Int))) (Dict.Dict Int (List (Int, Int)))
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStorage storage ->
            ( { model | storage = storage }, Cmd.none )

        SelectMode mode ->
            ( { model | mode = mode }, Cmd.none )

        Spawn tetrimino ->
            ( spawn tetrimino model, Cmd.none )

        SetControl control ->
            ( if Just control /= model.control then
                move { model | control = Just control } control

              else
                model
            , Cmd.none
            )
        CancelControl control ->
            ( if Just control == model.control then
                { model | control = Nothing }

              else
                model
            , Cmd.none
            )

        Move control ->
            ( move model control, Cmd.none )

        Rotate ->
            ( rotate model, Cmd.none )

        Advance ->
            advance model

        Place ->
            place model

        SettleBoard rowsToClear colsToClear ->
            let
                newLines = Dict.size rowsToClear + Dict.size colsToClear
                newModel = { model
                                        | gridState = Dict.foldl settleRows model.gridState rowsToClear 
                                                                    |> (\grid -> Dict.foldl settleCols grid colsToClear)
                                        , state = if newLines == 0 then Playing else Settling
                                        , lines = model.lines + newLines
                                        , clearing = Set.empty
                                    }
            in
            if newLines == 0
                then (newModel, Random.generate Spawn Tetrimino.random)
                else clearLines newModel
            

        Restart ->
            ( { model
                | gridState = Grid.empty ( 20, 20 )
                , state = Playing
                , lines = 0
              }
            , Cmd.batch
                [ Random.generate Spawn Tetrimino.random
                , if model.lines > 0 then
                    Storage.storeHighScore model.lines

                  else
                    Cmd.none
                ]
            )




spawn : Tetrimino -> Model -> Model
spawn tetrimino model =
    case spawnHelp 1 (Cell.spawn tetrimino) model.gridState of
        Ok newGrid ->
            { model | gridState = newGrid }

        Err _ ->
            { model | state = GameOver }


spawnHelp : Int -> List ( ( Int, Int ), Cell ) -> Grid Cell -> Result Foul (Grid Cell)
spawnHelp count cells grid =
    case groupInsert cells grid of
        ( Just CellTaken, _ ) ->
            if count > 0 then
                spawnHelp (count - 1) (List.map (Tuple.mapFirst Grid.up) cells) grid

            else
                Err CellTaken

        ( _, newGrid ) ->
            Ok newGrid


activeCells : Grid Cell -> List ( ( Int, Int ), Cell )
activeCells grid =
    Grid.filter (\_ -> not << Cell.isSettled) grid
        |> Grid.toList


move : Model -> Control -> Model
move model control =
    case moveHelp control model.gridState of
        Ok newGrid ->
            { model | gridState = newGrid }

        Err _ ->
            model


place : Model -> ( Model, Cmd Msg )
place model =
    if List.isEmpty (activeCells model.gridState) then
        ( model, Cmd.none )

    else
        clearLines { model | gridState = placeHelp model.gridState }


placeHelp : Grid Cell -> Grid Cell
placeHelp grid =
    case moveHelp Down grid of
        Ok newGrid ->
            placeHelp newGrid

        Err _ ->
            grid


advance : Model -> ( Model, Cmd Msg )
advance model =
    case moveHelp Down model.gridState of
        Ok newGrid ->
            ( { model | gridState = newGrid }, Cmd.none )

        Err _ ->
            clearLines model


moveHelp : Control -> Grid Cell -> Result Foul (Grid Cell)
moveHelp control grid =
    let
        activePositionMap =
            activeCells grid
                |> List.map
                    (\( pos, _ ) ->
                        case control of
                            Left ->
                                ( pos, Grid.left pos )

                            Right ->
                                ( pos, Grid.right pos )

                            Down ->
                                ( pos, Grid.down pos )
                    )
    in
        case groupUpdate activePositionMap grid of
            ( Nothing, newGrid ) ->
                Ok newGrid

            ( Just foul, _ ) ->
                Err foul


clearLines : Model -> ( Model, Cmd Msg )
clearLines model =
    let
        coords = 
            Grid.coordinates model.gridState

        rows = 
            List.foldl
                (\( x, y ) -> Dict.update y (\mv -> Just (Maybe.withDefault [] mv ++ [( x, y )])))
                Dict.empty
                (coords)

        rowsToClear = 
            Dict.filter 
                (\_ row -> List.all (\c -> List.member c (Grid.positions model.gridState)) row )
                rows

        cols = 
            List.foldl
                (\( x, y ) -> Dict.update x (\mv -> Just (Maybe.withDefault [] mv ++ [( x, y )])))
                Dict.empty
                (coords)

        colsToClear =
            Dict.filter 
                (\_ col -> List.all (\c -> List.member c (Grid.positions model.gridState)) col )
                cols

        coordsToClear = 
            List.concat <| ((Dict.values rowsToClear) ++ (Dict.values colsToClear))

        gridAfterClear =
            List.foldl
                (\c -> Grid.filter (\pos _ -> pos /= c))
                model.gridState
                coordsToClear
    in
    ( { model
        | gridState = Grid.map (\_ -> Cell.settle) gridAfterClear
        , clearing = Set.fromList <| List.concat <| Dict.values rowsToClear ++ Dict.values colsToClear
        , state = Settling
      }
    , Process.sleep 500
        |> Task.perform (\_ -> SettleBoard rowsToClear colsToClear)
    )

settleRows : Int -> List (Int, Int) -> Grid Cell -> Grid Cell
settleRows _ clearedRow grid =
    let
        aboveClearedPositionMap =
            Grid.positions grid
                |> List.filterMap
                    (\pos ->
                        let
                            (x,y) = pos
                        in 
                            if
                                List.any (\(cx,cy)->x==cx && y < cy) clearedRow
                            then
                                Just ( pos, Grid.left pos )
                            else
                                Nothing
                        )
    in
    case groupUpdate aboveClearedPositionMap grid of
        ( Nothing, newGrid ) ->
            newGrid

        ( Just _, _ ) ->
            grid

settleCols : Int -> List (Int, Int) -> Grid Cell -> Grid Cell
settleCols _ clearedCol grid =
    let
        aboveClearedPositionMap =
            Grid.positions grid
                |> List.filterMap
                    (\pos ->
                        let
                            (x,y) = pos
                        in 
                            if
                                List.any (\(cx,cy)->x < cx && y == cy) clearedCol
                            then
                                Just ( pos, Grid.right pos )
                            else
                                Nothing
                        )
    in
        case groupUpdate aboveClearedPositionMap grid of
            ( Nothing, newGrid ) ->
                newGrid

            ( Just _, _ ) ->
                grid


rotate : Model -> Model
rotate model =
    case rotateHelp model.gridState of
        Ok newGrid ->
            { model | gridState = newGrid }

        Err _ ->
            model


rotateHelp : Grid Cell -> Result Foul (Grid Cell)
rotateHelp grid =
    let
        center =
            List.filter (Cell.isCenter << Tuple.second) (activeCells grid)
                |> List.head

        rotatePositionMap =
            activeCells grid
                |> List.map
                    (\( pos, _ ) ->
                        ( pos
                        , case center of
                            Just ( cPos, _ ) ->
                                Tetrimino.rotateAround pos cPos

                            Nothing ->
                                pos
                        )
                    )
    in
    case groupUpdate rotatePositionMap grid of
        ( Nothing, newGrid ) ->
            Ok newGrid

        ( Just foul, _ ) ->
            Err foul


groupInsert : List ( ( Int, Int ), Cell ) -> Grid Cell -> ( Maybe Foul, Grid Cell )
groupInsert cells grid =
    ( foulCheck (List.map Tuple.first cells) grid
    , List.foldl (\( pos, cell ) -> Grid.insert pos cell) grid cells
    )


groupUpdate : List ( ( Int, Int ), ( Int, Int ) ) -> Grid Cell -> ( Maybe Foul, Grid Cell )
groupUpdate positionMap grid =
    let
        ( oldPositions, newPositions ) =
            List.unzip positionMap

        gridWithoutOld =
            Grid.filter (\pos _ -> not <| List.member pos oldPositions) grid
    in
    ( foulCheck newPositions gridWithoutOld
    , positionMap
        |> List.foldl
            (\( pos, newPos ) newGrid ->
                case
                    ( Grid.get pos grid, List.member pos newPositions )
                of
                    ( Just cell, True ) ->
                        Grid.insert newPos cell newGrid

                    ( Just cell, False ) ->
                        Grid.remove pos newGrid
                            |> Grid.insert newPos cell

                    ( Nothing, _ ) ->
                        newGrid
            )
            grid
    )


foulCheck : List ( Int, Int ) -> Grid Cell -> Maybe Foul
foulCheck positions grid =
    let
        coords =
            Grid.coordinates grid

        outOfBounds ( x, y ) =
            not (List.any (\(cx,cy)-> cx==x && cy==y) coords)
    in
        if List.any (\pos -> Grid.member pos grid) positions then
            Just CellTaken

        else if List.any outOfBounds positions then
            Just OutOfBounds

        else
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        gameSpeed =
            case model.mode of
                Normal ->
                    1800 - (model.lines * 4)

                Medium ->
                    600 - (model.lines * 2)

                Hard ->
                    200 - model.lines
    in
    Sub.batch
        [ case model.control of
            Just _ ->
                Sub.none
            Nothing ->
                Time.every (toFloat gameSpeed) (\_ -> Advance)
            
        , case model.control of
            Just control ->
                Time.every 100 (\_ -> Move control)
            Nothing ->
                Sub.none

        , Storage.changes GotStorage

        , Browser.Events.onKeyDown
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                Decode.succeed (SetControl Left)
                            "ArrowRight" ->
                                Decode.succeed (SetControl Right)
                            "ArrowUp" ->
                                Decode.succeed Rotate
                            "ArrowDown" ->
                                Decode.succeed (SetControl Down)
                            " " ->
                                Decode.succeed Place
                            _ ->
                                Decode.fail ""
                    )
            )

        , Browser.Events.onKeyUp
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                Decode.succeed (CancelControl Left)
                            "ArrowRight" ->
                                Decode.succeed (CancelControl Right)
                            "ArrowDown" ->
                                Decode.succeed (CancelControl Down)
                            _ ->
                                Decode.fail ""
                    )
            )
        ]


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
