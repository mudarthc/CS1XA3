module SketchPad exposing (..)

import Html exposing (Html,Attribute,program,div,input,text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import Platform.Sub as S
import Platform.Cmd as C
import Keyboard exposing (KeyCode,downs)
import Svg exposing (Svg,Attribute,svg,polyline,circle, rect)
import Svg.Attributes exposing (width,height,fill,points,stroke,cx,cy,r,x,y,rx,ry)


main : Program Never Model Msg
main = program
  { init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }

type alias Model =
  {
    coordinate : String,
    x : Int,
    y : Int
  }

type Msg = KeyMsg KeyCode

init : (Model,Cmd Msg)
init =  (model,C.none)

model : Model
model = Model "400,550" 400 550

moveU : Model -> Model
moveU model = { model | y = model.y - 10 }

moveD : Model -> Model
moveD model = { model | y = model.y + 10 }

moveR : Model -> Model
moveR model = { model | x = model.x + 10 }

moveL : Model -> Model
moveL model = { model | x = model.x - 10 }

next : Model -> Model
next model = { model | coordinate = model.coordinate ++ " " ++ (toString model.x) ++ "," ++ (toString model.y) }

new : Model -> Model
new model = { model | y = 300, x = 300 }

update : Msg -> Model -> (Model,Cmd Msg)
update msg  model  =
    case msg of
        KeyMsg 87 -> (moveU model,C.none)
        KeyMsg 83 -> (moveD model,C.none)
        KeyMsg 68 -> (moveR model,C.none)
        KeyMsg 65 -> (moveL model,C.none)
        KeyMsg 13 -> (next model,C.none)
        KeyMsg 82 -> (new model,C.none)
        _ -> (model,C.none)

subscriptions : Model -> Sub Msg
subscriptions model = downs KeyMsg

view : Model -> Html Msg
view model =
  let
    point = model.coordinate
    posX = toString model.x
    posY = toString model.y
  in
    div [style
  [
    ("height","100%"),
    ("width","100%"),
    ("font-size","20px"),
    ("text-align","center")
  ]]
    [
      div [headerStyle] [text "Connect The Dots"],
      div [style
  [
    ("height","90%"),
    ("width","100%")
  ]]
      [
        div [sideStyle]
        [
          div [] [text ("x: " ++ posX)],
          div [] [text ("y: " ++ posY)],
          div [style [("padding","10px 10px"), ("font-size","30px")]] [text "code:"],
          div [style [("color", "#76D7C4")]] [text "teal"],
          div [] [text "->"],
          div [style [("color", "#2B2727")]] [text "black"],
          div [] [text "->"],
          div [style [("color", "#B40707")]] [text "red"],
          div [] [text "->"], 
          div [style [("color", "#27AE60")]] [text "green"],
          div [] [text "->"],
          div [style [("color", "#E67E22")]] [text "orange"],
          div [] [text "->"],
          div [style [("color", "#8E44AD")]] [text "purple"],
          div [] [text "->"],
          div [style [("color", "#2C51B4")]] [text "blue"],
          div [] [text "->"],
          div [style [("color", "#F5B7B1")]] [text "pink"],
          div [] [text "->"],
          div [style [("color", "#F4D03F")]] [text "yellow"],
          div [] [text "->"],
          div [style [("color", "#2B2727")]] [text "black"]
        ],
        svg [width "90%", height "100%"]
        [
          circle [cx posX, cy posY, r "10", fill "black"] [],
          polyline [points point, fill "none",stroke "black"] [],
          circle [cx "600", cy "40", r "5", fill "#8E44AD"] [],
          circle [cx "525", cy "200", r "5", fill "#2C51B4"] [],
          circle [cx "675", cy "200", r "5", fill "#E67E22"] [],
          circle [cx "900", cy "250", r "5", fill "#27AE60"] [],
          circle [cx "300", cy "250", r "5", fill "#F1948A"] [],
          circle [cx "500", cy "350", r "5", fill "#F4D03F"] [],
          circle [cx "700", cy "350", r "5", fill "#B40707"] [],
          circle [cx "600", cy "450", r "5", fill "#76D7C4"] [],
          circle [cx "800", cy "550", r "5", fill "#2B2727"] [],
          circle [cx "400", cy "550", r "5", fill "#2B2727"] []

        ]
      ]
    ]

headerStyle =
  style
  [
    ("height","10%"),
    ("width","100%"),
    ("background-color","#707070"),
    ("font-size","50px")
  ]
headerStyles =
  style
  [
    ("height","100%"),
    ("width","100%"),
    ("font-size","20px"),
    ("text-align","center")
  ]


mainStyle =
  style
  [
    ("height","90%"),
    ("width","100%")
  ]

sideStyle =
  style
  [
    ("height","100%"),
    ("width","10%"),
    ("float","left"),
    ("background-color","#DCDCDC")
  ]
