import Html exposing (Html, Attribute)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Effects exposing (Effects, Never)
import Random exposing (int, Seed)
import StartApp as StartApp
import Task exposing (Task)
import Signal.Time as Time2
import Signal exposing (Signal)
import Time as Time
import Array as Array
import Window as Window
import Json.Decode as Json


-- Time2.startTime, open question: how do I use this without polling for the time,
-- e.g. just have a signal that goes off once?
startTimeSeed : Signal Action
startTimeSeed = Signal.map ((\x -> Init x) << Random.initialSeed << round) (Time.every 100)

windowDimensions : Signal Action
windowDimensions = Signal.map (\x -> Dimensions x) Window.dimensions

app =
    StartApp.start { init = init, view = view, update = update, inputs = [startTimeSeed, windowDimensions] }

names = Array.fromList [
  "タフェー",
  "トゥーン",
  "ヘス",
  "ゴ",
  "ユ",
  "トウ",
  "みなみ",
  "シリン",
  "ペック",
  "セシリア",
  "トィン",
  "ヨーキン",
  "ジョズエ",
  "ジュ",
  "ゆみ",
  "マラティーナ",
  "カイ",
  "あき",
  "カ",
  "マイケル"]

main =
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks

-- MODEL
type alias Model = 
  { name : Maybe String
  , seed : Maybe Seed
  , dimensions : Maybe (Int, Int)
  }


init : (Model, Effects Action)
init = ({ name = Maybe.Nothing, dimensions = Maybe.Nothing, seed = Maybe.Nothing }, Effects.none)

type Action = DoNothing | Init Seed | Tap | Dimensions (Int, Int)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    DoNothing -> (model, Effects.none)
    Dimensions x -> ({model | dimensions = Just x }, Effects.none)
    Tap -> case model.seed of
      Nothing -> (model, Effects.none)
      Just s -> let randomSeedPair = (Random.generate (int 0 ((Array.length names) - 1)) s)
                in ( { model | name = (Array.get (fst randomSeedPair) names)
                             , seed = Just (snd randomSeedPair) }
                   , Effects.none)
    Init s -> 
      case model.seed of
        Nothing -> ({ model | seed = Just s }, Effects.none)
        Just _ -> (model, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model = 
  let displayText = case model.name of
        Maybe.Nothing -> "タップしてください"
        Maybe.Just n -> n
  in Html.div [onTouchDown address, onMouseDown address] [Html.text displayText]

onTouchDown : Signal.Address Action -> Attribute
onTouchDown address =
    on "touchstart" Json.value (\_ -> Signal.message address Tap)

onMouseDown : Signal.Address Action -> Attribute
onMouseDown address =
    on "mousedown" Json.value (\_ -> Signal.message address Tap)