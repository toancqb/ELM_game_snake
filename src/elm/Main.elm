module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode
import List exposing (..)
import Tuple exposing (..)
import Random
import Functions

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }

type alias Position = (Int, Int)
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , coloredSquare : Int
  , constDirection : (Int, Int)
  , speed: Int  
  , directions: List Position
  , snake: List Position
  , fruits: List Position
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 0 cRight 400 [cRight,cRight,cRight,cRight] [(0,3), (0,2), (0,1), (0,0)] [(6,6), (30,30)]
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}

updateDirections : Position -> List (Int, Int) -> List Position
updateDirections p ps =
      [p] ++ (Functions.inits ps) 

updateSnake : List Position -> List Position -> List Position
updateSnake lp ld =
  case (lp, ld) of
      ([], []) -> []
      ([], _) -> [] 
      (_, []) -> []
      (x::xs, y::ys) -> (Tuple.first x + Tuple.first y, Tuple.second x + Tuple.second y) :: (updateSnake xs ys)


updateSquare : Model -> Model
updateSquare ({ snake, directions, fruits} as model)  =
  if head snake == head fruits then    
    let
      newHead = Maybe.withDefault (0,0) (head fruits)
    in
      {model | 
      snake = (Tuple.first newHead + (Tuple.first model.constDirection)*2, Tuple.second newHead + (Tuple.second model.constDirection)*2) :: (updateSnake snake directions), 
      directions = model.constDirection :: (updateDirections model.constDirection model.directions), 
      fruits = List.reverse fruits}
  else 
    {model | snake = updateSnake snake directions, directions = updateDirections model.constDirection model.directions}


toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  case Debug.log "key" key of
    Space -> update ToggleGameLoop model
    ArrowUp -> ({model | constDirection = cUp}, Cmd.none)
    ArrowDown -> ({model | constDirection = cDown}, Cmd.none)
    ArrowLeft -> ({model | constDirection = cLeft}, Cmd.none)
    ArrowRight -> ({model | constDirection = cRight}, Cmd.none)
    --_ -> Update.none model

cLeft : (Int, Int)
cLeft = (0, -1)

cRight : (Int, Int)
cRight = (0, 1)

cUp : (Int, Int)
cUp = (-1, 0)

cDown : (Int, Int)
cDown = (1, 0)

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= model.speed then
    updateSquare model
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of    
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model

{-| Manage all your view functions here. -}
cell : Int -> Int -> Html msg
cell index active =
  let class = if active == index then "cell active" else "cell" in
  Html.div [ Attributes.class class ] []



movingSquare : Model -> Html msg
movingSquare { coloredSquare } =
  Html.div [ Attributes.class "grid" ]     
      (List.map (\x -> cell x coloredSquare) (List.range 0 1599))
  
isContain : List Position -> Int -> Bool
isContain ps n = List.any (\x -> x == n) (List.map Functions.positionToSquareColored ps)

cells : Int -> List Position -> List Position -> Html msg
cells index actives fruits =
      --let class = if isContain actives index then "cell active" else "cell" in
      --Html.div [ Attributes.class class ] []
      if isContain actives index then 
        Html.div [ Attributes.class "cell active" ] []
      else if isContain fruits index then
        Html.div [ Attributes.class "cell fruit" ] []
      else
        Html.div [ Attributes.class "cell" ] []

movingSquares : Model -> Html msg
movingSquares { snake, fruits } =
  Html.div [ Attributes.class "grid" ]     
      (List.map (\x -> cells x snake (List.take 1 fruits)) (List.range 0 1599))

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ 
    --  Html.img [ Attributes.src "/logo.svg" ] []
    --, explanations model
    --, 
    movingSquares model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown


subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
