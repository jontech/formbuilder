port module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, onMouseUp, onMouseDown)
import Svg exposing (svg, line)
import Svg
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth, strokeLinecap, viewBox, preserveAspectRatio)
import Json.Decode as Decode exposing (Decoder, map2, field, float, int)
import Dict exposing (Dict)

-- PORTS


port elemFromTo : ((Int, Int), (Int, Int)) -> Cmd msg
port elemFromToUpdate : ((String, String) -> msg) -> Sub msg                   


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
    { value : String }

    
type alias Paragraph =
    { value : String }

        
type alias Line =
    { starts : (Int, Int)
    , ends : (Int, Int)
    }

    
type alias Model =
  { content : String
  , textFields : Dict String TextField
  , paragraphs : Dict String Paragraph
  , mouse : MouseMoveData
  , connections : List Line
  , drawStart : Maybe (Int, Int)
  , editing : Bool
  , sourceTarget : List (String, String)
  }


init : () -> ( Model, Cmd msg )
init _ =
  ({ content = ""
  , textFields = Dict.empty
  , paragraphs = Dict.empty
  , mouse = MouseMoveData 0 0
  , connections = []
  , drawStart = Nothing
  , editing = False
  , sourceTarget = []
  }, Cmd.none)


-- UPDATE


decoder : Decoder MouseMoveData
decoder =
    map2 MouseMoveData
        (field "clientX" int)
        (field "clientY" int)


type Msg
  = Change String String
  | NewTextField
  | NewParagraph
  | MouseMove MouseMoveData
  | DrawStart
  | DrawEnd
  | ElemFromTo (String, String)
  | EditMode


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Change id newContent ->
        -- find p in Dict by `name` of input element and update its value
        ({ model | paragraphs =
               List.filter (\(src, trg) -> src == id) model.sourceTarget
                    |> (List.foldr
                            (\(src, trg) res -> Dict.update trg (Maybe.map (\v -> Paragraph (v.value ++ newContent))) res)
                 model.paragraphs)
         }, Cmd.none)

    NewTextField ->
        ({ model | textFields = Dict.insert "1" (TextField "") model.textFields
         }, Cmd.none)

    NewParagraph ->
        ({ model | paragraphs = Dict.insert "2" (Paragraph "Some text") model.paragraphs
         }, Cmd.none)

    MouseMove data ->
        ({ model | mouse = data }, Cmd.none)

    DrawStart ->
        let
            start = (model.mouse.offsetX, model.mouse.offsetY)
        in
            ({ model | drawStart = Just start }, Cmd.none)

    DrawEnd ->
        case model.drawStart of
             Just (startX, startY) ->
                 let
                     start = (startX, startY)
                     end = (model.mouse.offsetX, model.mouse.offsetY)
                 in
                     ({ model | connections = (Line start end) :: model.connections
                      , drawStart = Nothing
                      }
                     , elemFromTo (start, end)
                     )
             Nothing ->
                 (model, Cmd.none)
            
    ElemFromTo (startId, endId) ->
        ( { model | sourceTarget = (Debug.log "fromto" (startId, endId)) :: model.sourceTarget }
        , Cmd.none
        )

    EditMode ->
        ({ model | editing = if (model.editing) then False else True }, Cmd.none)


-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ = elemFromToUpdate ElemFromTo


-- VIEW


createTextField : String -> TextField -> Html Msg
createTextField elemId textField = 
    input [ placeholder "Some text"
          , id elemId
          , value textField.value
          , onInput (Change elemId)
          ] []

        
createParagraph : String -> Paragraph -> Html msg
createParagraph elemId paragraph =
    p [ id elemId ] [ text paragraph.value ]


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
    div [] [ div [ class "controls"
                 ] [ button [ onClick NewTextField ] [ text "New text field" ]
                   , button [ onClick NewParagraph ] [ text "New paragraph" ]
                   , button [ onClick EditMode ] [ text "Edit mode" ]
                   ]
           , div [ class "canvas"
                 , on "mousemove" (Decode.map MouseMove decoder)
                 , onMouseDown DrawStart
                 , onMouseUp DrawEnd
                 ] [ svg [ height 1000
                         , width 1000
                         , viewBox "0 0 1000 1000"
                         ]
                         ((case model.drawStart of
                               Just (x, y) -> 
                                   drawLine (x, y) (model.mouse.offsetX, model.mouse.offsetY)
                               Nothing ->
                                   text ""
                          ) :: (List.map (\connect -> drawLine connect.starts connect.ends) model.connections))
                   , div [] (List.map (\(id, tf) -> createTextField id tf) (Dict.toList model.textFields))
                   , div [] (List.map (\(id, p) -> createParagraph id p) (Dict.toList model.paragraphs))
                   ]
           ]
