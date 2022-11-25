module Main exposing (..)

import Color as ElmColor
import Playground exposing (Computer, Shape, circle, game, move, rectangle, red, rgb, toX, toY)


type alias Snake =
    { length : Int
    , x : Float
    , y : Float
    , segments : List { x : Float, y : Float }
    , xspeed : Float
    , yspeed : Float
    }


initialSnake : Snake
initialSnake =
    { length = 500
    , x = 0
    , y = 0
    , segments = [ { x = -100.0, y = 20.0 } ]
    , xspeed = 0.0
    , yspeed = 0.0
    }


main =
    game view update initialSnake


hsl h s l =
    let
        { red, green, blue } =
            ElmColor.hsl h s l |> ElmColor.toRgba
    in
    rgb (255 * red) (255 * green) (255 * blue)


backgroundColor =
    hsl 0.15 0.4 0.2


snakeColor =
    hsl 0.4 0.6 0.5


view : Computer -> Snake -> List Shape
view computer { x, y } =
    [ rectangle backgroundColor computer.screen.width computer.screen.height
    , circle snakeColor 20 |> move x y
    ]


update : Computer -> Snake -> Snake
update computer snake =
    { snake
        | x =
            snake.x
                + snake.xspeed
                |> clamp (-computer.screen.width / 2) (computer.screen.width / 2)
        , y =
            snake.y
                + snake.yspeed
                |> clamp (-computer.screen.height / 2) (computer.screen.height / 2)
        , xspeed = snake.xspeed + toX computer.keyboard
        , yspeed = snake.yspeed + toY computer.keyboard
    }
