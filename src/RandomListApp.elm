import Html exposing (Html, Attribute)
import Html.Attributes exposing (style, src)
import Html.Events exposing (onClick)
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

names = Array.fromList 
  [ ("マルティナ", "🐰")
  , ("マイケル", "🐦")
  , ("ジュ", "🍮")
  , ("アキ", "🐹")
  , ("セシリア", "😃")
  , ("カ", "😃")
  , ("ミナミ", "🐰")
  , ("シリン", "🐰")
  , ("トゥエン", "😃")
  , ("トゥーン", "😃")
  , ("ヘス", "🐼")
  , ("トウ", "😃")
  , ("ヨーキン", "🐺")
  , ("ユ", "🐤")
  , ("ゴ", "🐰")
  , ("ホスエ", "😼")
  , ("カイ", "😃")
  , ("ゆみ", "🐰")
  , ("ペック", "😃")
  , ("タフィー", "🍬")
  ]

main =
    app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks

-- MODEL
type alias Model = 
  { nameAndImg : Maybe (String, String)
  , seed : Maybe Seed
  , dimensions : Maybe (Int, Int)
  }


init : (Model, Effects Action)
init = ({ nameAndImg = Maybe.Nothing, dimensions = Maybe.Nothing, seed = Maybe.Nothing }, Effects.none)

type Action = DoNothing | Init Seed | Tap | Dimensions (Int, Int)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    DoNothing -> (model, Effects.none)
    Dimensions x -> ({model | dimensions = Just x }, Effects.none)
    Tap -> case model.seed of
      Nothing -> (model, Effects.none)
      Just s -> let randomSeedPair = (Random.generate (int 0 ((Array.length names) - 1)) s)
                in ( { model | nameAndImg = (Array.get (fst randomSeedPair) names)
                             , seed = Just (snd randomSeedPair) }
                   , Effects.none)
    Init s -> 
      case model.seed of
        Nothing -> ({ model | seed = Just s }, Effects.none)
        Just _ -> (model, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model = 
  let children = case model.nameAndImg of
        Maybe.Nothing -> [Html.span [] [Html.text "タップして"]]
        Maybe.Just (n, img) -> [Html.span [] [Html.text n], Html.span [] [Html.text (img)]]
  in Html.div [onClick address Tap] children
