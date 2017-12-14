module Main exposing (..)

import Board exposing (..)
import Html exposing (..)
import Html.Attributes exposing (action)
import Html.Events exposing (..)


-- Model


type alias Model =
    { boardSideHeight : Int
    , mineCount : Int
    , board : Board.Model
    }


initModel : Model
initModel =
    { boardSideHeight = 12
    , mineCount = 12
    , board = Board.initModel
    }



-- Update


type MainMsg
    = InputBoardSize String
    | InputMinesCount String
    | CreateNewGame
    | BoardMessage Board.Msg


update : MainMsg -> Model -> ( Model, Cmd MainMsg )
update msg model =
    case msg of
        CreateNewGame ->
            let
                ( newBoard, newCommand ) =
                    Board.updateBoard
                        model.mineCount
                        model.boardSideHeight
            in
            ( { model
                | board = newBoard
              }
            , Cmd.map BoardMessage newCommand
            )

        BoardMessage boardMsg ->
            let
                ( newBoard, newCommand ) =
                    Board.update boardMsg model.board
            in
            ( { model
                | board = newBoard
              }
            , Cmd.map BoardMessage newCommand
            )

        InputBoardSize value ->
            let
                newModel =
                    case String.toInt value of
                        Ok val ->
                            { model
                                | boardSideHeight = val
                            }

                        Err err ->
                            { model
                                | boardSideHeight = 1
                            }
            in
            ( newModel, Cmd.none )

        InputMinesCount value ->
            let
                newModel =
                    case String.toInt value of
                        Ok val ->
                            { model
                                | mineCount = val
                            }

                        Err err ->
                            { model
                                | mineCount = 1
                            }
            in
            ( newModel, Cmd.none )



-- View


view : Model -> Html MainMsg
view model =
    Html.form [ action "#" ]
        [ input [ onInput InputBoardSize ] []
        , input [ onInput InputMinesCount ] []
        , button [ onClick CreateNewGame ] [ text "Create" ]
        , Html.map BoardMessage (Board.view model.board)
        ]


main : Program Never Model MainMsg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = update CreateNewGame initModel
        }
