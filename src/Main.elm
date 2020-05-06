port module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, onMouseUp, onMouseDown)
import Svg exposing (svg, line)
import Svg
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth, strokeLinecap, viewBox, preserveAspectRatio)
import Json.Decode as Decode exposing (Decoder, map2, field, float, int)


-- PORTS


port elemNameFromPoint : (Int, Int) -> Cmd msg
port elemNameUpdate : (String -> msg) -> Sub msg                   


-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    }

    
type alias TextField =
    { name : String
    , value : String
    }

    
type alias Paragraph =
    { value : String }

        
type alias Line =
    { starts : (Int, Int)
    , ends : (Int, Int)
    }

    
type alias Model =
  { content : String
  , textFields : List TextField
  , paragraphs : List Paragraph
  , mouse : MouseMoveData
  , connections : List Line
  , drawStart : Maybe (Int, Int)
  , name : String
  , editing : Bool
  }


init : () -> ( Model, Cmd msg )
init _ =
  ({ content = ""
  , textFields = []
  , paragraphs = []
  , mouse = MouseMoveData 0 0
  , connections = []
  , drawStart = Nothing
  , name = ""
  , editing = False       
  }, Cmd.none)


-- UPDATE


decoder : Decoder MouseMoveData
decoder =
    map2 MouseMoveData
        (field "clientX" int)
        (field "clientY" int)


type Msg
  = Change String
  | NewTextField
  | NewParagraph
  | MouseMove MouseMoveData
  | Draw
  | ElemNameUpdate String
  | EditMode


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Change newContent ->
        ({ model | content = newContent ++ model.content }, Cmd.none)
    NewTextField ->
        ({ model | textFields = (TextField "testName" "") :: model.textFields }, Cmd.none)
    NewParagraph ->
        ({ model | paragraphs = (Paragraph "Some text") :: model.paragraphs }, Cmd.none)
    MouseMove data ->
        ({ model | mouse = data }, Cmd.none)
    Draw ->
        ({ model | connections =
              case model.drawStart of
                  Just (x, y) ->
                      (Line (x, y) (model.mouse.offsetX, model.mouse.offsetY) :: model.connections)
                  Nothing ->
                      model.connections
        , drawStart =
              case model.drawStart of
                  Nothing ->
                      Just (model.mouse.offsetX, model.mouse.offsetY)
                  _ ->
                      Nothing
        }, elemNameFromPoint (model.mouse.offsetX, model.mouse.offsetY))

    ElemNameUpdate name ->
        ({ model | name = name }, Cmd.none)
    EditMode ->
        ({ model | editing = if (model.editing) then False else True }, Cmd.none)


-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ = elemNameUpdate ElemNameUpdate


-- VIEW


createTextField : TextField -> Html Msg
createTextField textField = 
    input [ placeholder "Some text"
          , name textField.name
          , value textField.value
          , onInput Change
          ] []

        
createParagraph : Paragraph -> Html msg
createParagraph paragraph =
    p [] [ text paragraph.value ]


drawLine : (Int, Int) -> (Int, Int) -> Svg.Svg msg
drawLine (startx, starty) (endx, endy) =
    line [ x1 (String.fromInt startx)
         , y1 (String.fromInt starty)
         , x2 (String.fromInt endx)
         , y2 (String.fromInt endy)
         , stroke "blue"
         , strokeWidth "10"
         , strokeLinecap "round"
         ] []
    
        
view : Model -> Html Msg
view model =
    div [ class (String.join " " [ "canvas"
                                 ])
        , on "mousemove" (Decode.map MouseMove decoder)
        , onClick Draw
        ] [ svg [ height 1000
                , width 1000
                , viewBox "0 0 1000 1000"
                ]
                ((case model.drawStart of
                      Just (x, y) -> 
                          drawLine (x, y) (model.mouse.offsetX, model.mouse.offsetY)
                      Nothing ->
                          text "") :: (List.map
                                           (\connect -> drawLine connect.starts connect.ends)
                                           model.connections))
          , button [ onClick NewTextField ] [ text "New text field" ]
          , button [ onClick NewParagraph ] [ text "New paragraph" ]
          , button [ onClick EditMode ] [ text "Edit mode" ]                       
          , div [] (List.map (\tf -> createTextField tf) model.textFields)
          , div [] [ text (String.reverse model.content) ]
          , div [] (List.map (\p -> createParagraph p) model.paragraphs)
          ]
