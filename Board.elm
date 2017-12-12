module Board exposing (..)

import Array exposing (Array, initialize, repeat, toList)
import Html exposing (..)
import PopulateBoard exposing (populate)
import Random exposing (..)
import Random.Array exposing (..)


-- Model


type alias Model =
    { hiddenBoard : Array.Array String
    , boardSideHeight : Int
    , mineCount : Int
    }


initModel : Model
initModel =
    { hiddenBoard = repeat 9 "0"
    , boardSideHeight = 3
    , mineCount = 1
    }



-- Update


type Msg
    = CreateNewGame
    | ShuffledList (Array String)


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
        { hiddenBoard = repeat boardLength "0"
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



-- View


stringSplit : String -> Int -> List String
stringSplit string chunckSize =
    if String.length string > 0 then
        List.append
            [ String.left chunckSize string ]
            (stringSplit (String.dropLeft chunckSize string) chunckSize)
    else
        []


view : Model -> Html Msg
view model =
    div []
        [ stringSplit (populate model.hiddenBoard 0) model.boardSideHeight
            |> List.map (\e -> li [] [ text e ])
            |> ul []
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
