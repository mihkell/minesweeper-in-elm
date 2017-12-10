module PopulateBoard exposing (..)

import Array exposing (Array, initialize, repeat, toList)


outOfRange : Int -> ( Int, Int ) -> Bool
outOfRange range ( pos_x, pos_y ) =
    pos_x < 0 || pos_y < 0 || pos_x >= range || pos_y >= range


indexByCord : ( Int, Int ) -> Int -> Int
indexByCord ( pos_x, pos_y ) boardHeight =
    let
        x =
            pos_x

        y =
            pos_y * boardHeight

        pointOutOfRange =
            outOfRange boardHeight
    in
    if pointOutOfRange ( pos_x, pos_y ) then
        -1
    else
        x + y


positionToCoordinate : Int -> Int -> ( Int, Int )
positionToCoordinate position boardSideLength =
    let
        x =
            position % boardSideLength

        y =
            position // boardSideLength
    in
    ( x, y )


neighbors : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
neighbors ( position_x, position_y ) direction =
    ( position_x + Tuple.first direction
    , position_y + Tuple.second direction
    )


isMine : Array String -> Int -> ( Int, Int ) -> Bool
isMine board position direction =
    let
        boardHeight =
            floor (sqrt (toFloat (Array.length board)))

        positionCoordinate =
            positionToCoordinate position boardHeight

        neigbhorPosition =
            neighbors positionCoordinate direction
    in
    case
        Array.get
            (indexByCord neigbhorPosition boardHeight)
            board
    of
        Just val ->
            if val == "*" then
                True
            else
                False

        Nothing ->
            False


findNearbyMines : Array String -> Int -> Int
findNearbyMines board position =
    let
        surroundingCells =
            Array.fromList
                [ ( -1, -1 )
                , ( 0, -1 )
                , ( 1, -1 )
                , ( -1, 0 )
                , ( 1, 0 )
                , ( -1, 1 )
                , ( 0, 1 )
                , ( 1, 1 )
                ]
    in
    Array.length
        (Array.filter (isMine board position) surroundingCells)


populate : Array String -> Int -> String
populate board index =
    case Array.get index board of
        Just val ->
            if "*" == val then
                "*" ++ populate board (index + 1)
            else
                toString (findNearbyMines board index)
                    ++ populate board (index + 1)

        Nothing ->
            ""
