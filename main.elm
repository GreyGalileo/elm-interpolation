import Browser
import Browser.Events as E
import Json.Decode as D
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Element
import Html.Events.Extra.Mouse as Mouse


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
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model []
  , Cmd.none
  )



-- UPDATE


type Msg
  = NewPoint Point
  | Clear


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Clear -> (
      Model ([])
      ,Cmd.none
      )

    NewPoint point ->
      ( Model (point::model.points)
      , Cmd.none
      )



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
point_to_str: Point -> String
point_to_str (x,y) =
  "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ") "
 

list_points : List Point -> String
list_points list =
  (List.map point_to_str list) |> List.foldl (++) "" 

view : Model -> Html Msg
view model =
  div 
    [
    Mouse.onDown (\n -> NewPoint n.screenPos),
    style "background-color" "yellow",
    style "height" fill,
    style "width" fill
    ]

    [ h1 [] [ text ("Click anywhere") ],
    p [] [text (list_points model.points)],
    button [onClick (Clear) ] [text ("Clear Points")]
    ]