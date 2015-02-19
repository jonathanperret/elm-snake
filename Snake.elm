import Signal(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color(..)
import Time(..)
import Keyboard

type alias Arrows = { x : Int, y : Int }

type Update = ArrowChange Arrows | TimeDelta Float

screen : { width : Int, height : Int }
screen = { width = 600, height = 400 }
bounds = { xmin = -0.5 * toFloat screen.width, xmax = 0.5 * toFloat screen.width,
           ymin = -0.5 * toFloat screen.height, ymax = 0.5 * toFloat screen.height }

backgroundColor = (hsl 0.3 0.4 0.4)

type alias Snake = { x : Float, y : Float, xspeed : Float, yspeed : Float }

speed = 50.0

initialSnake : Snake
initialSnake = { x = -100.0, y = 20.0, xspeed = 0.0, yspeed = 0.0 }

main : Signal Element
main = map render snakes

snakes : Signal Snake
snakes = foldp movedSnake initialSnake updates

movedSnake : Update -> Snake -> Snake
movedSnake u snake = case u of
  ArrowChange a -> { snake | xspeed <- speed * toFloat (a.x), yspeed <- speed * toFloat (a.y) }
  TimeDelta dt ->  { snake | x <- clamp bounds.xmin bounds.xmax (snake.x + dt * snake.xspeed),
                             y <- clamp bounds.ymin bounds.ymax (snake.y + dt * snake.yspeed) }

directions : Signal Arrows
directions = dropIf (\{x,y}->x==0 && y==0) { x=1,y=0 } <|
  map (\k->case k of
    37 -> { x = -1, y = 0 }
    39 -> { x = 1, y = 0 }
    38 -> { x = 0, y = 1 }
    40 -> { x = 0, y = -1 }
    _ -> { x = 0, y = 0 }
  ) Keyboard.lastPressed


updates : Signal Update
updates = merge
  (map ArrowChange (directions))
  (map (TimeDelta << inSeconds) (fps 60))

render : Snake -> Element
render snake = collage screen.width screen.height [ background, move (snake.x, snake.y) snakeForm ]

background = filled backgroundColor <| rect (toFloat screen.width) (toFloat screen.height)

snakeForm : Form
snakeForm = filled green <| circle 12.0
