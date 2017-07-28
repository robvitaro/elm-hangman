module Hangman exposing (..)

import Update.Extra exposing (andThen)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Set


type GameState
    = Start
    | Playing
    | Won
    | Lost


type alias Model =
    { word : String
    , wordLetters : List Char
    , guess : Char
    , guessedLetters : List Char
    , strikes : Int
    , gameState : GameState
    }


initialModel : GameState -> Model
initialModel state =
    { word = ""
    , wordLetters = []
    , guess = ' '
    , guessedLetters = []
    , strikes = 0
    , gameState = state
    }



-- UPDATE


type Msg
    = NewGame
    | NewWord Int
    | MakeGuess Char
    | DetermineGameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( initialModel Playing, Random.generate NewWord (Random.int 1 100) )

        NewWord randNum ->
            let
                newWord =
                    String.toUpper (randomWord randNum)
            in
                ( { model
                    | word = newWord
                    , wordLetters = createWordLetters newWord
                  }
                , Cmd.none
                )

        MakeGuess value ->
            ( { model
                | guess = value
                , guessedLetters = addGuessedLetter value model.guessedLetters
                , strikes = model.strikes + (isItAStrike value model.wordLetters)
              }
            , Cmd.none
            )
                |> andThen update DetermineGameState

        DetermineGameState ->
            let
                wordGuessed =
                    (wholeWordGuessed model.guessedLetters model.wordLetters)

                state =
                    if wordGuessed then
                        Won
                    else if model.strikes >= 6 then
                        Lost
                    else
                        Playing
            in
                ( { model | gameState = state }, Cmd.none )


randomWord : Int -> String
randomWord randNum =
    case randNum of
        1 ->
            "press"

        2 ->
            "nut"

        3 ->
            "noxious"

        4 ->
            "handy"

        5 ->
            "wilderness"

        6 ->
            "rain"

        7 ->
            "name"

        8 ->
            "heal"

        9 ->
            "carriage"

        10 ->
            "stage"

        11 ->
            "respect"

        12 ->
            "thank"

        13 ->
            "good"

        14 ->
            "piquant"

        15 ->
            "succinct"

        16 ->
            "red"

        17 ->
            "previous"

        18 ->
            "approval"

        19 ->
            "type"

        20 ->
            "degree"

        21 ->
            "pizzas"

        22 ->
            "fork"

        23 ->
            "regret"

        24 ->
            "dear"

        25 ->
            "courageous"

        26 ->
            "rely"

        27 ->
            "iron"

        28 ->
            "adorable"

        29 ->
            "messy"

        30 ->
            "across"

        31 ->
            "alcoholic"

        32 ->
            "crush"

        33 ->
            "waves"

        34 ->
            "bulb"

        35 ->
            "grease"

        36 ->
            "donkey"

        37 ->
            "rural"

        38 ->
            "change"

        39 ->
            "tax"

        40 ->
            "camera"

        41 ->
            "panicky"

        42 ->
            "television"

        43 ->
            "tense"

        44 ->
            "difficult"

        45 ->
            "check"

        46 ->
            "juggle"

        47 ->
            "disastrous"

        48 ->
            "automatic"

        49 ->
            "shade"

        50 ->
            "size"

        51 ->
            "linen"

        52 ->
            "frame"

        53 ->
            "sky"

        54 ->
            "wealthy"

        55 ->
            "move"

        56 ->
            "march"

        57 ->
            "fool"

        58 ->
            "hole"

        59 ->
            "jeans"

        60 ->
            "uttermost"

        61 ->
            "jam"

        62 ->
            "seashore"

        63 ->
            "strip"

        64 ->
            "like"

        65 ->
            "ignorant"

        66 ->
            "painful"

        67 ->
            "dull"

        68 ->
            "dad"

        69 ->
            "walking"

        70 ->
            "feigned"

        71 ->
            "superb"

        72 ->
            "business"

        73 ->
            "plate"

        74 ->
            "disturbed"

        75 ->
            "instrument"

        76 ->
            "connection"

        77 ->
            "sail"

        78 ->
            "steep"

        79 ->
            "endurable"

        80 ->
            "shirt"

        81 ->
            "apparel"

        82 ->
            "bells"

        83 ->
            "helpless"

        84 ->
            "absurd"

        85 ->
            "guess"

        86 ->
            "possible"

        87 ->
            "phobic"

        88 ->
            "ruin"

        89 ->
            "rub"

        90 ->
            "egg"

        91 ->
            "smooth"

        92 ->
            "offend"

        93 ->
            "horses"

        94 ->
            "oatmeal"

        95 ->
            "tangy"

        96 ->
            "advice"

        97 ->
            "gray"

        98 ->
            "intelligent"

        99 ->
            "fact"

        100 ->
            "mourn"

        _ ->
            "XX"


createWordLetters : String -> List Char
createWordLetters word =
    String.toList word
        -- remove duplicates
        |> Set.fromList
        -- back to a list!
        |> Set.toList


theWord : String -> List Char -> String
theWord word guessedLetters =
    String.map
        (\c ->
            if List.member c guessedLetters then
                c
            else
                '_'
        )
        word


addGuessedLetter : Char -> List Char -> List Char
addGuessedLetter letter currentList =
    letter
        :: currentList
        -- remove duplicates
        |> Set.fromList
        -- back to a list!
        |> Set.toList


isItAStrike : Char -> List Char -> Int
isItAStrike letter wordLetters =
    if List.member letter wordLetters then
        0
    else
        1


letterGuessed : List Char -> Char -> Bool
letterGuessed guessedLetters letter =
    List.member letter guessedLetters


wholeWordGuessed : List Char -> List Char -> Bool
wholeWordGuessed guessedLetters wordLetters =
    List.all (letterGuessed guessedLetters) wordLetters



-- VIEW


aButton : msg -> String -> Html msg
aButton msg name =
    button [ class "primary", onClick msg ] [ text name ]


viewNewGameButton : GameState -> Html Msg
viewNewGameButton state =
    case state of
        Start ->
            aButton NewGame "Let's play!"

        Playing ->
            text ""

        Won ->
            aButton NewGame "Play again?"

        Lost ->
            aButton NewGame "Try again?"


guessLetterButton : List Char -> Char -> Html Msg
guessLetterButton guessedLetters letter =
    let
        isDisabled =
            List.member letter guessedLetters
    in
        button [ onClick (MakeGuess letter), disabled isDisabled ] [ text (String.fromChar letter) ]


viewGuessLetterButtons : List Char -> GameState -> Html Msg
viewGuessLetterButtons guessedLetters state =
    let
        allLetters =
            [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]

        buttons =
            List.map (guessLetterButton guessedLetters) allLetters
    in
        case state of
            Start ->
                text ""

            _ ->
                ul [] buttons

viewLostLetter :  List Char -> Char -> Html Msg
viewLostLetter  guessedLetters letter =         
    if List.member letter guessedLetters then
        span [] [text (String.fromChar letter)]
    else
        span [class "lost"] [text (String.fromChar letter)]


theLostWord : String -> List Char -> Html Msg
theLostWord word guessedLetters =
    let
        listOfLetters = String.toList word
    in
        div [] (List.map (viewLostLetter guessedLetters) listOfLetters)


viewWord : String -> List Char -> GameState -> Html Msg
viewWord word guessedLetters state =
    case state of
        Start ->
            text ""
        Playing ->
            div []
                [ h1 [] [ text (theWord word guessedLetters) ] ]
        Won ->
            div []
                [ h1 [class "won"] [ text (theWord word guessedLetters) ] ]        
        Lost ->
            div []
                [ h1 [] [ theLostWord word guessedLetters ] ]

viewMan : Int -> Html Msg
viewMan strikes = 
    div []
        [ img [ src ((toString strikes) ++ ".gif") ] [] ]

viewGameOver : GameState -> Html Msg
viewGameOver state =
    case state of
        Start ->
            div []
                [ h1 [] [ text "" ] ]

        Playing ->
            div []
                [ h1 [] [ text "" ] ]

        Won ->
            div []
                [ h1 [ class "won" ] [ text "You won!" ] ]

        Lost ->
            div []
                [ h1 [ class "lost" ] [ text "You lost!" ] ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "HANGMAN" ] ]
        , div []
            [ viewMan model.strikes
            , div [class "actionBox"] [viewGameOver model.gameState]
            , div [class "actionBox"] [ viewNewGameButton model.gameState ]
            , div [class "actionBox"] [viewWord model.word model.guessedLetters model.gameState]
            , viewGuessLetterButtons model.guessedLetters model.gameState
            -- , div [ class "debug" ] [ text (toString model) ]
            ]

        
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel Start, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
