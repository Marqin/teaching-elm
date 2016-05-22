-- elm package install elm-lang/html
-- elm package install evancz/elm-graphics
-- elm package install elm-lang/animation-frame


import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)

import Random
import Time exposing (Time)

import Color exposing (..)
import Collage exposing (collage, filled, circle, move, rect)
import Element

import AnimationFrame

main = Html.program
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }

---------------------------- MODEL ----------------------------

type alias Model = { position : (Float, Float), direction : (Float, Float),
                     speed : Float, lastTime : Time, physicsStep : Float}

---------------------------- INIT ----------------------------

init : (Model, Cmd Msg)
init = (Model (0, 0) (1, 0.75) 3.5 0 0.02 , Cmd.none) -- 100 fps for physics

---------------------------- UPDATE ----------------------------

type Msg = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ((moveBall model newTime), Cmd.none)

---------------------------- SUBSCRIPTIONS ----------------------------

subscriptions : Model -> Sub Msg
subscriptions model = AnimationFrame.times Tick

--------------------------------- VIEW --------------------------------

view : Model -> Html Msg
view model =
  div [ ]
  [
    h1 [] [text <| toString model.direction ] ,
    Element.toHtml <| collage 400 400
    [
      filled lightBlue <| (rect 400 400),
      move model.position <| filled black <| (circle 5)
    ]
  ]

---------------------------------- OTHER ----------------------------------

moveBall : Model -> Float -> Model
moveBall model newTime =
  let
    timeDiff = (newTime-model.lastTime)/1000.0
    newModel = iteration model ( floor (timeDiff / model.physicsStep))
  in
  { newModel | lastTime = newTime }


tupleMap : (a -> b) -> (a, a) -> (b, b)
tupleMap fn tpl = (fn (fst tpl), fn (snd tpl))

tupleSum tpl1 tpl2 = (fst tpl1 + fst tpl2, snd tpl1 + snd tpl2)


newDir axis position direction =
  if
    ((axis position >= 200) && (axis direction > 0) )
    ||
    ( (axis position <= -200) && (axis direction < 0) )
  then
    -1.0 * axis direction
  else
    axis direction

iteration : Model -> Int -> Model
iteration model acc =
  if acc < 0 || model.lastTime == 0 then
    model
  else
    let
      newDirectionX = newDir fst model.position model.direction
      newDirectionY = newDir snd model.position model.direction
      posDiff = tupleMap (\x -> x * model.speed) model.direction
      newPosition = tupleSum model.position posDiff
    in
    iteration {model | position = newPosition, direction = (newDirectionX, newDirectionY)} (acc-1)
