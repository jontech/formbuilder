port module Main exposing (..)

import Browser

import Html exposing (Html, div, input, text, button, nav)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, on, onMouseUp, onMouseDown)
import Svg exposing (svg, line)

import Svg.Attributes exposing (x1, y1, x2, y2, stroke, strokeWidth, strokeLinecap, viewBox)
import Json.Decode as Decode exposing (Decoder, field, int, string, decodeString)
import Json.Encode as Encode
import Dict exposing (Dict)


-- PORTS


port elemFromTo : ((Int, Int), (Int, Int)) -> Cmd msg
port elemFromToUpdate : (String -> msg) -> Sub msg

port updateCanvas : () -> Cmd msg
port canvasUpdate : (String -> msg) -> Sub msg

port saveState : String -> Cmd msg

port loadState : () -> Cmd msg
port loadStateUpdate : (String -> msg) -> Sub msg

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
    , pos : (Int, Int)
    }


type alias ElementAttr =
    { id : String
    , offsetTop : Int
    , offsetLeft : Int
    , clientWidth : Int
    , clientHeight : Int
    }


type alias FromTo =
    { start : ElementAttr
    , end : ElementAttr
    }


type alias Line =
    { starts : (Int, Int)
    , ends : (Int, Int)
    }

    
type alias Model =
  { elements : Dict String SeqElement
  , mouse : MouseMoveData
  , sourceTargetDrawing : List Line
  , drawStart : Maybe (Int, Int)
  , drawEnd : Maybe (Int, Int)
  , editing : Bool
  , sourceTarget : List (String, String)
  , canvas : ElementAttr
  , draging : Maybe Element
  }


type Msg
  = Change String String
  | MouseMove MouseMoveData
  | DrawStart
  | DrawEnd
  | ElemFromTo String
  | EditMode
  | NewCanvas String
  | DragStart Element
  | DragEnd
  | Save
  | Load
  | StateUpdate String
  | Reset


-- INIT


init : () -> ( Model, Cmd msg )
init _ =
    ( { elements = Dict.empty
      , mouse = MouseMoveData 0 0
      , sourceTargetDrawing = []
      , drawStart = Nothing
      , drawEnd = Nothing
      , editing = False
      , sourceTarget = []
      , canvas = ElementAttr "" 0 0 0 0
      , draging = Nothing
      }
    , updateCanvas ()
    )


-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ elemFromToUpdate ElemFromTo
              , canvasUpdate NewCanvas
              , loadStateUpdate StateUpdate
              ]


-- JSON


mouseDecoder : Decoder MouseMoveData
mouseDecoder =
    Decode.map2 MouseMoveData
        (field "clientX" int)
        (field "clientY" int)


elementDecoder : Decoder ElementAttr
elementDecoder =
    Decode.map5 ElementAttr
        (field "id" string)
        (field "offsetTop" int)
        (field "offsetLeft" int)
        (field "clientWidth" int)
        (field "clientHeight" int)


fromToDecoder : Decoder FromTo
fromToDecoder =
    Decode.map2 FromTo
        (field "start" elementDecoder)
        (field "end" elementDecoder)


modelEncoder : Model -> Encode.Value
modelEncoder model =
    Encode.object
        [ ( "elements"
          , Encode.dict
              identity
              (\seqElem ->
                   case seqElem.elem of
                       TextField ->
                           Encode.object
                               [ ("seq", Encode.int seqElem.seq)
                               , ("posX", Encode.int (Tuple.first seqElem.pos))
                               , ("posY", Encode.int (Tuple.second seqElem.pos))
                               ]
                       Paragraph text ->
                           Encode.object
                               [ ("text", Encode.string text)
                               , ("seq", Encode.int seqElem.seq)
                               , ("posX", Encode.int (Tuple.first seqElem.pos))
                               , ("posY", Encode.int (Tuple.second seqElem.pos))
                               ]
               )
               model.elements
          )
        , ( "sourceTargetDrawing"
          , Encode.list
              (\line ->
                   Encode.object
                   [ ("startX", Encode.int (Tuple.first line.starts))
                   , ("startY", Encode.int (Tuple.second line.starts))
                   , ("endX", Encode.int (Tuple.first line.ends))
                   , ("endY", Encode.int (Tuple.second line.ends))
                   ]
              )
              model.sourceTargetDrawing
          )
        , ( "sourceTarget"
          , Encode.list
              (\fromTo ->
                   Encode.object
                   [ ("from", Encode.string (Tuple.first fromTo))
                   , ("to", Encode.string (Tuple.second fromTo))
                   ]
              )
              model.sourceTarget
          )
        ]


elementsDecoder : Decoder (Dict String SeqElement)
elementsDecoder =
    Decode.dict (Decode.oneOf   -- ordering: more complex goes first
                     [ Decode.map3
                           (\text seq pos -> SeqElement seq (Paragraph text) pos)
                           (field "text" string)
                           (field "seq" int)
                           (Decode.map2
                                Tuple.pair
                                (field "posX" int)
                                (field "posY" int)
                           )
                     , Decode.map2
                            (\seq pos -> SeqElement seq TextField pos)
                            (field "seq" int)
                            (Decode.map2
                                 Tuple.pair
                                 (field "posX" int)
                                 (field "posY" int)
                            )
                     ]
                )


sourceTargetDrawingDecoder : Decoder (List Line)
sourceTargetDrawingDecoder =
    Decode.list (Decode.map2
                     Line
                     (Decode.map2
                          Tuple.pair
                          (field "startX" int)
                          (field "startY" int)
                     )
                     (Decode.map2
                          Tuple.pair
                          (field "endX" int)
                          (field "endY" int)
                     )
                )


sourceTargetDecoder : Decoder (List (String, String))
sourceTargetDecoder =
    Decode.list (Decode.map2
                     Tuple.pair
                     (field "from" string)
                     (field "to" string)
                )


modelDecoder : Model -> Decoder Model
modelDecoder model =
    Decode.map3
        (\elements sourceTargetDrawing sourceTarget ->
             { model | elements = elements
             , sourceTargetDrawing = sourceTargetDrawing
             , sourceTarget = sourceTarget
             }
        )
        (field "elements" elementsDecoder)
        (field "sourceTargetDrawing" sourceTargetDrawingDecoder)
        (field "sourceTarget" sourceTargetDecoder)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Change id inputVal ->
        ({ model | elements =
               List.filter (\(src, _) -> src == id) model.sourceTarget
                    |> List.foldr
                       (\(_, trg) res -> Dict.update
                            trg
                            (Maybe.map (appendParagraph inputVal))
                            res
                       )
                       model.elements
         }, Cmd.none)

    DragStart elem ->
        ( { model | draging = Just elem
          , editing = True
          }
        , Cmd.none
        )

    DragEnd ->
        case model.draging of
            Just elem ->
                let
                    seq = nextSeq (Dict.values model.elements)
                in
                    ( { model | elements =
                            Dict.insert
                                (String.fromInt seq)
                                (SeqElement
                                     seq
                                     elem
                                     (model.mouse.clientX, model.mouse.clientY)
                                )
                                model.elements
                      , draging = Nothing
                      , editing = False
                      }
                    , Cmd.none
                    )
            Nothing ->
                ({ model | editing = False }, Cmd.none)

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
            
    ElemFromTo data ->
        case (decodeString fromToDecoder data, (model.drawStart, model.drawEnd)) of
            (Ok elem, (Just start, Just end)) ->
                let
                    new = (elem.start.id, elem.end.id)
                in
                    (if model.editing && validConnect new then
                         { model | sourceTarget = new :: model.sourceTarget
                         , sourceTargetDrawing = Line start end :: model.sourceTargetDrawing
                         , drawStart = Nothing
                         , drawEnd = Nothing
                         }
                     else
                         { model | drawStart = Nothing
                         , drawEnd = Nothing
                         }
                    , Cmd.none
                    )
            _ ->
                    ({ model | drawStart = Nothing
                     , drawEnd = Nothing
                     }
                    , Cmd.none
                    )

    EditMode ->
        ({ model | editing = not model.editing }, Cmd.none)

    NewCanvas data ->
        ( { model | canvas = case decodeString elementDecoder data of
                                 Ok canvas -> canvas
                                 Err _ -> model.canvas
          }
        , Cmd.none
        )

    Save ->
        ( model
        , modelEncoder model |> Encode.encode 0 |> saveState
        )

    Load ->
        ( model, loadState () )

    StateUpdate data ->
        ( case decodeString (modelDecoder model) data of
              Ok newModel ->
                  newModel 
              Err _ ->
                  model
        , Cmd.none
        )

    Reset ->
        init ()


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ] [
         let
             navStyle = "nav-link btn btn-outline-primary mr-1"
         in
         nav [ class "nav mb-2 justify-content-center" ]
             [ button [ class navStyle, onClick (DragStart TextField) ] [ text "New text field" ]
             , button [ class navStyle, onClick (DragStart (Paragraph "")) ] [ text "New paragraph" ]
             , button [ class navStyle, onClick EditMode ] [ text "Link elements" ]
             , button [ class navStyle, onClick Save ] [ text "Save" ]
             , button [ class navStyle, onClick Load ] [ text "Load" ]
             , button [ class navStyle, onClick Reset ] [ text "Reset" ]
             ]
        , div
             ([ id "canvas"
              , on "mousemove" (Decode.map MouseMove mouseDecoder)
              , onMouseDown DrawStart
              , onMouseUp DrawEnd                  
              , onClick DragEnd
              ] ++ if model.editing then [ style "cursor" "crosshair" ] else []
             )
             ([ renderDrawing model ] ++  renderElements model)
           ]


-- HELPERS


renderElements : Model -> List (Html.Html Msg)
renderElements model =
     List.map
         (\(elemId, seqElem) ->
              let
                  pos = canvasOffset model seqElem.pos
                  px = \a -> a ++ "px"
              in
                  case seqElem.elem of
                      Paragraph val ->
                          div [ class "card m-2"
                              , style "position" "absolute"
                              , Tuple.first pos |> String.fromInt |> px |> style "left"
                              , Tuple.second pos |> String.fromInt |> px |> style "top" 
                              ] [ div [ id elemId
                                      , onClick DrawEnd
                                      , class "card-body"
                                      ] [ text val ]
                                ]
                      TextField ->
                          input ([ placeholder "Type something"
                                 , id elemId
                                 , value ""
                                 , onInput (Change elemId)
                                 , class "m-2"
                                 , style "position" "absolute"
                                 , Tuple.first pos |> String.fromInt |> px |> style "left"
                                 , Tuple.second pos |> String.fromInt |> px |> style "top"
                                 ] ++ if model.editing then [style "cursor" "crosshair"] else []
                                ) []
         )
         (Dict.toList model.elements)


renderDrawing : Model -> Svg.Svg msg
renderDrawing model =
    let
        offset = canvasOffset model
    in
        svg [ height model.canvas.clientHeight
            , width model.canvas.clientWidth
            , viewBox ("0 0 "
                           ++  String.fromInt model.canvas.clientWidth
                           ++ " "
                           ++  String.fromInt model.canvas.clientHeight)
            ]
        ((case model.drawStart of
              Just (x, y) -> 
                  drawLine (offset (x, y)) (offset (model.mouse.clientX, model.mouse.clientY))
              Nothing ->
                  text ""
         ) :: List.map
                  (\connect -> drawLine (offset connect.starts) (offset connect.ends))
                  model.sourceTargetDrawing)


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
