module Board exposing (..)

import Array exposing (Array, initialize, repeat, toList)
import Html exposing (..)
import Html.Events exposing (onClick)
import PopulateBoard exposing (populate)
import Random exposing (..)
import Random.Array exposing (..)


-- Model


type alias Model =
    { hiddenBoard : Array.Array String
    , boardSideHeight : Int
    , mineCount : Int
    , lastClickedValue : Int
    }


initModel : Model
initModel =
    { hiddenBoard = repeat 9 "0"
    , boardSideHeight = 3
    , mineCount = 1
    , lastClickedValue = -1
    }



-- Update


type Msg
    = CreateNewGame
    | ShuffledList (Array String)
    | ClickedOnIndex Int


boardsize : Array String -> Int
boardsize board =
    Array.length board


updateBoard : Int -> Int -> ( Model, Cmd Msg )
updateBoard mineCount boardSideHeight =
    let
        boardLength =
            boardSideHeight * boardSideHeight
    in
    update CreateNewGame
        { initModel
            | hiddenBoard = repeat boardLength "0"
            , boardSideHeight = boardSideHeight
            , mineCount = mineCount
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateNewGame ->
            let
                shuffledBoard =
                    Array.append
                        (repeat model.mineCount "*")
                        (repeat (boardsize model.hiddenBoard - model.mineCount) "0")
                        |> Random.Array.shuffle
            in
            ( model, Random.generate ShuffledList shuffledBoard )

        ShuffledList list ->
            ( { model
                | hiddenBoard = list
              }
            , Cmd.none
            )

        ClickedOnIndex index ->
            ( { model
                | lastClickedValue = index
              }
            , Cmd.none
            )



-- View


stringSplit : Int -> String -> List String
stringSplit chunckSize string =
    if String.length string > 0 then
        List.append
            [ String.left chunckSize string ]
            (stringSplit chunckSize (String.dropLeft chunckSize string))
    else
        []


createHiddenBoardSquare : Model -> Int -> String -> List (Html Msg)
createHiddenBoardSquare model rowNumber rowText =
    stringSplit 1 rowText
        |> List.indexedMap
            (\ix ex ->
                span
                    [ onClick
                        (ClickedOnIndex
                            (ix + (rowNumber * model.boardSideHeight))
                        )
                    ]
                    [ text ex ]
            )


createBoardRow : Model -> Int -> String -> Html Msg
createBoardRow model rowNumber rowText =
    li [] (createHiddenBoardSquare model rowNumber rowText)


startIndex : Int
startIndex =
    0


createHiddenBoard : Model -> Html Msg
createHiddenBoard model =
    stringSplit model.boardSideHeight (populate model.hiddenBoard startIndex)
        |> List.indexedMap (createBoardRow model)
        |> ul []


view : Model -> Html Msg
view model =
    div []
        [ createHiddenBoard model
        , div []
            (List.map
                (\e -> p [] [ text e ])
                [ "'-' - not viewd;"
                , "  -- '0' free;"
                , " -- 1..8 mineCount;"
                , " -- '*' mine"
                ]
            )
        ]
