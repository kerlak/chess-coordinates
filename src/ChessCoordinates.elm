port module ChessCoordinates exposing (main)

import Html exposing (Html, div, text, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Browser
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Array

port save : String -> Cmd msg

saveToStorage : Model -> ( Model, Cmd Msg )
saveToStorage model =
    ( model, save (encode 2 model) )

main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = \value ->
                ( value
                    |> Decode.decodeValue decode
                    |> Result.withDefault initial
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

initial : Model
initial =
    Model (Coordinate -1 -1) 0 0 0

encode : Int -> Model -> String
encode indent model =
    Encode.encode
        indent
        (Encode.object
            [ ( "coordinate_x", Encode.int model.coordinate.x )
            , ( "coordinate_y", Encode.int model.coordinate.y )
            , ( "corrects", Encode.int model.corrects )
            , ( "ratio", Encode.float model.ratio )
            , ( "incorrects", Encode.int model.incorrects )
            ]
        )

decode : Decode.Decoder Model
decode =
    Decode.map5
        (\coordinate_x coordinate_y corrects ratio incorrects ->
            { initial
                | coordinate = Coordinate coordinate_x coordinate_y
                , corrects = corrects
                , ratio = ratio
                , incorrects = incorrects
            }
        )
        (Decode.field "coordinate_x" Decode.int)
        (Decode.field "coordinate_y" Decode.int)
        (Decode.field "corrects" Decode.int)
        (Decode.field "ratio" Decode.float)
        (Decode.field "incorrects" Decode.int)

type alias Coordinate = {
    x: Int,
    y: Int
    }


type alias Model =
    {
        coordinate: Coordinate,
        corrects: Int,
        ratio: Float,
        incorrects: Int
    }


type Msg
    = NewCoordinate Coordinate
    | RequestRandomCoordinate
    | SelectCoordinate Coordinate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewCoordinate coordinate ->
            ({model | coordinate = coordinate} |> saveToStorage)
        RequestRandomCoordinate ->
            (model, generate_random_coordinate)
        SelectCoordinate coordinate ->
            case model.coordinate.x < 0 || model.coordinate.y < 0 of
                True ->
                    (model, Cmd.none)
                False ->
                    case isSameCoordinate coordinate model.coordinate of
                        True ->
                            let corrects = model.corrects + 1
                            in ({model | corrects = corrects, ratio = toFloat corrects / (toFloat model.incorrects + toFloat corrects)}, generate_random_coordinate)
                        False ->
                            let incorrects = model.incorrects + 1
                            in ({model | incorrects = incorrects, ratio = toFloat model.corrects / (toFloat incorrects + toFloat model.corrects)} |> saveToStorage)

    
toFixed : Int -> Float -> String
toFixed num float =
    case num == 0 of
        True ->
            String.fromFloat float
        False ->
            let newValue = toFloat(round (float * toFloat(10 ^ num))) / toFloat(10 ^ num)
            in String.fromFloat newValue

view_scores : Model -> Html Msg
view_scores model =
    div [ class "score" ]
        [ div [ class "corrects noselect" ]
            [ text (String.fromInt model.corrects) ]
        , div [ class "ratio noselect" ]
            [ text (toFixed 2 model.ratio) ]
        , div [ class "incorrects noselect" ]
            [ text (String.fromInt model.incorrects) ]
        ]

view_coordinate : Model -> Html Msg
view_coordinate model =

    case model.coordinate.x < 0 || model.coordinate.y < 0 of
        True ->        
            div [ class "coordinate" ]
                [ h1 [ class "noselect", onClick RequestRandomCoordinate ]
                    [ text "Start learning" ]
                ]
        False ->
            div [ class "coordinate" ]
                [ h1 [ class "noselect" ]
                    [ text ( model.coordinate |> coordinates_to_string) ]
                ]

view_chessboard : Model -> Html Msg
view_chessboard model =
    div [ class "chessboard" ]
        [ div [ class "row piece" ]
            [ div [ class "square white black_rook" , onClick (SelectCoordinate (Coordinate 0 7)) ][]
            , div [ class "square black black_knight", onClick (SelectCoordinate (Coordinate 1 7)) ][]
            , div [ class "square white black_bishop", onClick (SelectCoordinate (Coordinate 2 7)) ][]
            , div [ class "square black black_queen", onClick (SelectCoordinate (Coordinate 3 7)) ][]
            , div [ class "square white black_king", onClick (SelectCoordinate (Coordinate 4 7)) ][]
            , div [ class "square black black_bishop", onClick (SelectCoordinate (Coordinate 5 7)) ][]
            , div [ class "square white black_knight", onClick (SelectCoordinate (Coordinate 6 7)) ][]
            , div [ class "square black black_rook", onClick (SelectCoordinate (Coordinate 7 7)) ][]
            ]
        , div [ class "row piece" ]
            [ div [ class "square black black_pawn", onClick (SelectCoordinate (Coordinate 0 6)) ][]
            , div [ class "square white black_pawn", onClick (SelectCoordinate (Coordinate 1 6)) ][]
            , div [ class "square black black_pawn", onClick (SelectCoordinate (Coordinate 2 6)) ][]
            , div [ class "square white black_pawn", onClick (SelectCoordinate (Coordinate 3 6)) ][]
            , div [ class "square black black_pawn", onClick (SelectCoordinate (Coordinate 4 6)) ][]
            , div [ class "square white black_pawn", onClick (SelectCoordinate (Coordinate 5 6)) ][]
            , div [ class "square black black_pawn", onClick (SelectCoordinate (Coordinate 6 6)) ][]
            , div [ class "square white black_pawn", onClick (SelectCoordinate (Coordinate 7 6)) ][]
            ]
        , div [ class "row" ]
            [ div [ class "square white", onClick (SelectCoordinate (Coordinate 0 5)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 1 5)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 2 5)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 3 5)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 4 5)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 5 5)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 6 5)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 7 5)) ][]
            ]
        , div [ class "row" ]
            [ div [ class "square black", onClick (SelectCoordinate (Coordinate 0 4)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 1 4)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 2 4)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 3 4)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 4 4)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 5 4)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 6 4)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 7 4)) ][]
            ]
        , div [ class "row" ]
            [ div [ class "square white", onClick (SelectCoordinate (Coordinate 0 3)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 1 3)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 2 3)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 3 3)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 4 3)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 5 3)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 6 3)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 7 3)) ][]
            ]
        , div [ class "row" ]
            [ div [ class "square black", onClick (SelectCoordinate (Coordinate 0 2)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 1 2)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 2 2)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 3 2)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 4 2)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 5 2)) ][]
            , div [ class "square black", onClick (SelectCoordinate (Coordinate 6 2)) ][]
            , div [ class "square white", onClick (SelectCoordinate (Coordinate 7 2)) ][]
            ]
        , div [ class "row piece" ]
            [ div [ class "square white white_pawn", onClick (SelectCoordinate (Coordinate 0 1)) ][]
            , div [ class "square black white_pawn", onClick (SelectCoordinate (Coordinate 1 1)) ][]
            , div [ class "square white white_pawn", onClick (SelectCoordinate (Coordinate 2 1)) ][]
            , div [ class "square black white_pawn", onClick (SelectCoordinate (Coordinate 3 1)) ][]
            , div [ class "square white white_pawn", onClick (SelectCoordinate (Coordinate 4 1)) ][]
            , div [ class "square black white_pawn", onClick (SelectCoordinate (Coordinate 5 1)) ][]
            , div [ class "square white white_pawn", onClick (SelectCoordinate (Coordinate 6 1)) ][]
            , div [ class "square black white_pawn", onClick (SelectCoordinate (Coordinate 7 1)) ][]
            ]
        , div [ class "row piece" ]
            [ div [ class "square black white_rook", onClick (SelectCoordinate (Coordinate 0 0)) ][]
            , div [ class "square white white_knight", onClick (SelectCoordinate (Coordinate 1 0)) ][]
            , div [ class "square black white_bishop", onClick (SelectCoordinate (Coordinate 2 0)) ][]
            , div [ class "square white white_queen", onClick (SelectCoordinate (Coordinate 3 0)) ][]
            , div [ class "square black white_king", onClick (SelectCoordinate (Coordinate 4 0)) ][]
            , div [ class "square white white_bishop", onClick (SelectCoordinate (Coordinate 5 0)) ][]
            , div [ class "square black white_knight", onClick (SelectCoordinate (Coordinate 6 0)) ][]
            , div [ class "square white white_rook", onClick (SelectCoordinate (Coordinate 7 0)) ][]
            ]
        ]

view : Model -> Html Msg
view model =
    div [ class "app_body"]
        [ view_chessboard model
        , view_coordinate model
        , view_scores model
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

isSameCoordinate : Coordinate -> Coordinate -> Bool
isSameCoordinate coordinate_1 coordinate_2 =
    coordinate_1.x == coordinate_2.x && coordinate_1.y == coordinate_2.y

coordinates_to_string : Coordinate -> String
coordinates_to_string coordinate =
    let
        letter = case ["a", "b", "c", "d", "e", "f", "g", "h"]
                            |> Array.fromList
                            |> Array.get coordinate.x
                            of
                    Nothing ->
                        ""
                    Just choosen_letter ->
                        choosen_letter
    in
        letter ++ String.fromInt((coordinate.y + 1))

coor : Random.Generator Int
coor =
  Random.int 0 7

generate_random_coordinate : Cmd Msg
generate_random_coordinate =
    Random.generate NewCoordinate (Random.map2 Coordinate coor coor)