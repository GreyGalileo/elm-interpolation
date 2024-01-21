import Browser
import Browser.Events as E
import Browser.Dom as Dom
import Task
import Json.Decode as D
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Svg
import Svg.Attributes as Svga


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Point =
 (Float, Float)
 


type alias Model =
  { points: List Point
  ,  svg_width: Float
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model [] 0
  , getSvgWidth
  )



-- UPDATE


type Msg
  = NewPoint Point
  | Clear
  |SvgWidth (Result Dom.Error Dom.Element)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> (
      Model [] model.svg_width
      ,Cmd.none
      )

    NewPoint point ->
      ( Model (point::model.points) model.svg_width
      , Cmd.none
      )
    
    SvgWidth result ->
      case result of
        Ok elem ->
          (Model model.points elem.element.width, Cmd.none)
        Err e ->
          (model, Cmd.none)
        



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getSvgWidth: Cmd Msg
getSvgWidth =
  Task.attempt SvgWidth (Dom.getElement "Graphs")

-- VIEW
point_to_str: Point -> String
point_to_str (x,y) =
  String.fromFloat x ++ "," ++ String.fromFloat y ++ " "
 
list_points : List Point -> String
list_points list =
  (List.map point_to_str list) |> List.foldl (++) "" 


point_to_svg: Point -> Svg.Svg msg
point_to_svg (x,y) = 
  Svg.circle [ 
    Svga.cx (String.fromFloat x),
    Svga.cy (String.fromFloat y),
    Svga.r "2", Svga.fill "black" ] []

build_li_aux: Float -> Point -> (Float->Float) -> (Float -> Float)
build_li_aux xi (x1,y1) li=
  if xi == x1 then (li) else
  (\x -> (x - x1) / (xi - x1) * (li x))
  
build_li: Float -> List Point -> (Float -> Float)
build_li xi points =
  List.foldl (build_li_aux xi) (\x -> 1) points

build_lagrange: List Point -> (Float -> Float)
build_lagrange points =
  List.foldl (\(xi,yi) f -> (\x ->  (f x) + yi * (build_li xi points) x)) (\x -> 0.0) points


divided_differences: List Point -> Float -> Float
divided_differences points x =
  0.0


view : Model -> Html Msg
view model =
  let lagrange: (Float -> Float) 
      lagrange = build_lagrange model.points in
  let path_string = (List.range 0 (ceiling model.svg_width) |> List.map (\a -> (toFloat a)) |> List.map (\x -> (x , lagrange x))|> list_points) 
  in
  div 
    [
    Mouse.onDown (\n -> NewPoint n.clientPos),
    style "background-color" "orange",
    style "height" "100vh",
    style "width" "100vw",
    style "margin" "0px"
    ]
    [
    --p [style "margin" "0px"] [text (list_points model.points)],
    Svg.svg[Svga.width "100%", Svga.height "94%", Svga.id "Graphs"]
    ([
      Svg.text "Click Anywhere",
      Svg.polyline [ Svga.fill "none", Svga.stroke "purple", Svga.points path_string ] []
    ] ++ (List.map point_to_svg model.points)) ,
    button [onClick (Clear) , style "height" "5%", style "margin" "0px"] [text ("Clear Points")]
    ]