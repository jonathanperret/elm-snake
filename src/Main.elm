module Main exposing (..)

import Color as ElmColor
import List
import Maybe
import Playground exposing (Computer, Shape, circle, game, move, rectangle, red, rgb, toX, toY)


type alias Snake =
    { length : Int
    , segments : List { x : Float, y : Float }
    , newdirection : { x : Float, y : Float }
    , direction : { x : Float, y : Float }
    , speed : Float
    }


initialSnake : Snake
initialSnake =
    { length = 150
    , segments = [ { x = 0.0, y = 0.0 } ]
    , newdirection = { x = 1, y = 0 }
    , direction = { x = 1, y = 0 }
    , speed = 2
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


gridSize =
    50


view : Computer -> Snake -> List Shape
view computer snake =
    rectangle backgroundColor computer.screen.width computer.screen.height
        :: List.map viewSegment snake.segments
        ++ viewHead snake

eyePos = 5

viewHead { segments, newdirection } =
    case segments of
        { x, y } :: _ ->
            [ circle (rgb 0 0 0) 10 |> move (x + eyePos * newdirection.x) (y + eyePos * newdirection.y) ]

        [] ->
            []


viewSegment { x, y } =
    circle snakeColor (gridSize * 0.4) |> move x y


snap x =
    x / gridSize |> truncate |> (*) gridSize |> toFloat


update : Computer -> Snake -> Snake
update computer snake =
    let
        { x, y } =
            List.head snake.segments
                |> Maybe.withDefault { x = 0, y = 0 }

        arrows =
            [ computer.keyboard.up
            , computer.keyboard.down
            , computer.keyboard.left
            , computer.keyboard.right
            ]

        onGrid =
            modBy gridSize (round x)
                == 0
                && modBy gridSize (round y)
                == 0

        newdirection =
            case arrows of
                [ True, False, False, False ] ->
                    if snake.direction.y < 0 then
                        snake.direction

                    else
                        { x = 0, y = 1 }

                [ False, True, False, False ] ->
                    if snake.direction.y > 0 then
                        snake.direction

                    else
                        { x = 0, y = -1 }

                [ False, False, True, False ] ->
                    if snake.direction.x > 0 then
                        snake.direction

                    else
                        { x = -1, y = 0 }

                [ False, False, False, True ] ->
                    if snake.direction.x < 0 then
                        snake.direction

                    else
                        { x = 1, y = 0 }

                _ ->
                    snake.newdirection

        direction =
            if onGrid then
                snake.newdirection

            else
                snake.direction
    in
    { snake
        | segments =
            { x =
                x
                    + direction.x
                    * snake.speed
                    |> clamp (-computer.screen.width / 2 |> snap) (computer.screen.width / 2 |> snap)
            , y =
                y
                    + direction.y
                    * snake.speed
                    |> clamp (-computer.screen.height / 2 |> snap) (computer.screen.height / 2 |> snap)
            }
                :: List.take snake.length snake.segments
        , newdirection = newdirection
        , direction = direction
    }
