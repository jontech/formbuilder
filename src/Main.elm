port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, Attribute, div, input, text, button, p, nav)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, onMouseUp, onMouseDown)
import Svg exposing (svg, line)
import Svg
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth, strokeLinecap, viewBox, preserveAspectRatio)
import Json.Decode as Decode exposing (Decoder, field, float, int, decodeString)
import Dict exposing (Dict)


-- PORTS


port elemFromTo : ((Int, Int), (Int, Int)) -> Cmd msg
port elemFromToUpdate : ((String, String) -> msg) -> Sub msg
port canvasUpdate : (String -> msg) -> Sub msg


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
    { clientX : Int
    , clientY : Int  
    }


type Element
    = TextField
    | Paragraph String


type alias SeqElement =
    { seq : Int
    , elem : Element
    }


type alias Canvas =
    { offsetTop : Int
    , offsetLeft : Int
    , clientWidth : Int
    , clientHeight : Int
    }
          

type alias Line =
    { starts : (Int, Int)
    , ends : (Int, Int)
    }

    
type alias Model =
  { content : String
  , textFields : Dict String SeqElement
  , paragraphs : Dict String SeqElement
  , mouse : MouseMoveData
  , sourceTargetDrawing : List Line
  , drawStart : Maybe (Int, Int)
  , drawEnd : Maybe (Int, Int)
  , editing : Bool
  , sourceTarget : List (String, String)
  , canvas : Canvas
  }


type Msg
  = Change String String
  | NewTextField
  | NewParagraph
  | MouseMove MouseMoveData
  | DrawStart
  | DrawEnd
  | ElemFromTo (String, String)
  | EditMode
  | NewCanvas String


-- INIT

init : () -> ( Model, Cmd msg )
init _ =
  ({ content = ""
   , textFields = Dict.singleton "1" { seq = 1, elem = TextField }
   , paragraphs = Dict.singleton "1" { seq = 1, elem = Paragraph ""}
   , mouse = MouseMoveData 0 0
   , sourceTargetDrawing = []
   , drawStart = Nothing
   , drawEnd = Nothing
   , editing = False
   , sourceTarget = []
   , canvas = Canvas 0 0 0 0
  }, Cmd.none)


-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ elemFromToUpdate ElemFromTo
                            , canvasUpdate NewCanvas
                            ]


-- JSON


decoder : Decoder MouseMoveData
decoder =
    Decode.map2 MouseMoveData
        (field "clientX" int)
        (field "clientY" int)


canvasDecoder : Decoder Canvas
canvasDecoder =
    Decode.map4 Canvas
        (field "offsetTop" int)
        (field "offsetLeft" int)
        (field "clientWidth" int)
        (field "clientHeight" int)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Change id inputVal ->
        ({ model | paragraphs =
               List.filter (\(src, _) -> src == id) model.sourceTarget
                    |> List.foldr
                       (\(_, trg) res -> Dict.update
                            trg
                            (Maybe.map (appendParagraph inputVal))
                            res
                       )
                       model.paragraphs
         }, Cmd.none)

    NewTextField ->
        let
            seq = nextSeq (Dict.values model.textFields)
        in
            ({ model | textFields = Dict.insert
                   (String.fromInt seq)
                   (SeqElement seq TextField)
                   model.textFields
             }
            , Cmd.none
            )

    NewParagraph ->
        let
            seq = nextSeq (Dict.values model.paragraphs)
        in
            ({ model | paragraphs = Dict.insert
                   (String.fromInt seq)
                   (SeqElement seq (Paragraph ""))
                   model.paragraphs
             }
            , Cmd.none
            )

    MouseMove data ->
        ({ model | mouse = data }, Cmd.none)

    DrawStart ->
        let
            start = (model.mouse.clientX, model.mouse.clientY)
        in
            (if model.editing then
                 { model | drawStart = Just start }
             else
                 model
            , Cmd.none
            )

    DrawEnd ->
        case model.drawStart of
             Just (startX, startY) ->
                 let
                     start = (startX, startY)
                     end = (model.mouse.clientX, model.mouse.clientY)
                 in
                     ({ model | drawEnd = Just end }, elemFromTo (start, end))
             Nothing ->
                 (model, Cmd.none)
            
    ElemFromTo new ->
        (if model.editing && (validConnect new) then
             case (model.drawStart, model.drawEnd) of
                 (Just start, Just end) ->
                     { model | sourceTarget = new :: model.sourceTarget
                     , sourceTargetDrawing = (Line start end) :: model.sourceTargetDrawing
                     , drawStart = Nothing
                     , drawEnd = Nothing
                     }
                 _ ->
                     { model | drawStart = Nothing
                     , drawEnd = Nothing
                     }
         else
             { model | drawStart = Nothing
             , drawEnd = Nothing
             }
        , Cmd.none
        )

    EditMode ->
        ({ model | editing = if (model.editing) then False else True }, Cmd.none)

    NewCanvas data ->
        ( { model | canvas = case (decodeString canvasDecoder data) of
                                 Ok canvas -> canvas
                                 Err _ -> Debug.log "error" model.canvas
          }
        , Cmd.none
        )


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ] [
         nav [ class "nav mb-2" ]
             [ button [ class "nav-link", onClick NewTextField ] [ text "New text field" ]
             , button [ class "nav-link", onClick NewParagraph ] [ text "New paragraph" ]
             , button [ class "nav-link", onClick EditMode ] [ text "Edit mode" ]
             ]
        , div [ id "canvas"
              , class (if model.editing then "edit-mode-on" else "")
              , on "mousemove" (Decode.map MouseMove decoder)
              , onMouseDown DrawStart
              , onMouseUp DrawEnd
              ] [ let
                    offset = canvasOffset model
                  in
                      svg [ height model.canvas.clientHeight
                          , width model.canvas.clientWidth
                          , viewBox ("0 0 " ++ (String.fromInt model.canvas.clientWidth) ++ " " ++ (String.fromInt model.canvas.clientHeight))
                          ]
                         ((case model.drawStart of
                               Just (x, y) -> 
                                   drawLine (offset (x, y)) (offset (model.mouse.clientX, model.mouse.clientY))
                               Nothing ->
                                   text ""
                          ) :: (List.map
                                    (\connect -> drawLine (offset connect.starts) (offset connect.ends))
                                    model.sourceTargetDrawing))
                   , div
                         [ class "row" ]
                         [ div [ class "col" ] (List.map
                                       (\(elemId, seqElem) ->
                                            case seqElem.elem of
                                                TextField ->
                                                    input [ placeholder "Type something"
                                                          , id elemId
                                                          , value ""
                                                          , onInput (Change elemId)
                                                          , class "m-2"
                                                          ] []
                                                _ -> text "")
                                       (Dict.toList model.textFields)
                                  )
                         , div [ class "col" ] (List.map
                                       (\(elemId, seqElem) ->
                                            case seqElem.elem of
                                                Paragraph val -> div [ class "card m-2" ] [
                                                                  div [ id elemId, class "card-body" ] [ text val ]
                                                                 ]
                                                _ -> text "")
                                       (Dict.toList model.paragraphs)
                                  )
                         ]
                   ]
           ]


-- HELPERS


nextSeq : List SeqElement -> Int
nextSeq elems =
    List.map (\elem -> elem.seq) elems |> List.maximum |> Maybe.withDefault 0 |> (+) 1

        
appendParagraph : String -> SeqElement -> SeqElement
appendParagraph newVal seqElem =
    { seqElem | elem = case seqElem.elem of
                           Paragraph val ->
                               Paragraph (val ++ newVal)
                           _ ->
                               seqElem.elem
     }


canvasOffset : Model -> (Int, Int) -> (Int, Int)
canvasOffset model (x, y) =
    ( x - model.canvas.offsetLeft
    , y - model.canvas.offsetTop
    )
        

validConnect : (String, String) -> Bool
validConnect (startId, endId) =
    (startId /= "canvas") && (endId /= "canvas")


drawLine : (Int, Int) -> (Int, Int) -> Svg.Svg msg
drawLine (startx, starty) (endx, endy) =
    line [ x1 (String.fromInt startx)
         , y1 (String.fromInt starty)
         , x2 (String.fromInt endx)
         , y2 (String.fromInt endy)
         , stroke "blue"
         , strokeWidth "4"
         , strokeLinecap "round"
         ] []
