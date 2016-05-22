-- elm package install elm-lang/html
-- elm package install evancz/elm-graphics
-- elm reactor


import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)

import Random
import Time exposing (Time, second)

import Color exposing (..)
import Collage exposing (collage, filled, square, rotate)
import Element

main = Html.program
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }

---------------------------- MODEL ----------------------------

type alias Model = { dieFace : Int, timeDiff : Time }

---------------------------- INIT ----------------------------

init : (Model, Cmd Msg)
init = (Model 0 0, Cmd.none)

---------------------------- UPDATE ----------------------------

type Msg = Roll | NewFace Int | Tick Time
-- Random.generate : (a -> msg) -> Random.Generator a -> Cmd msg
-- Random.int : Int -> Int -> Random.Generator Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ({model | timeDiff = 0}, Random.generate NewFace (Random.int 1 6))
    NewFace newFace ->
      ({model | dieFace = newFace}, Cmd.none)
    Tick newTime ->
      ({model | timeDiff = model.timeDiff + 1} , Cmd.none)

---------------------------- SUBSCRIPTIONS ----------------------------

subscriptions : Model -> Sub Msg
subscriptions model = Time.every second Tick

--------------------------------- VIEW --------------------------------

view : Model -> Html Msg
view model =
  div []
  [
    h1 [] [ text <| diceText model ],
    button [ onClick Roll ] [ text "Roll" ],
    br [] [],
    br [] [],
    text ("You haven't rolled for: " ++ toString model.timeDiff ++ " seconds!"),
    Element.toHtml <| collage 400 400 [rotate (degrees -5*model.timeDiff) <| filled red <| (square 50)]
  ]

---------------------------------- OTHER ----------------------------------

diceText model =
  if model.dieFace == 0 then "Roll the dice!" else (toString model.dieFace)
