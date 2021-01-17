module Utils exposing (parseFloat, roundDollar, renderIntegerInput, renderFloatInput)

import Round
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (step, type_, value)


-- Math


parseFloat : String -> Float
parseFloat value =
    value |> String.toFloat |> Maybe.withDefault 0.0


roundDollar : Float -> String
roundDollar value =
    value |> Round.round 2 |> (++) "$"


-- View



renderIntegerInput : String -> String -> (String -> msg) -> Html msg
renderIntegerInput label data msg =
    div []
        [ span [] [ text label ]
        , input [ onInput msg, value data, type_ "number" ] []
        ]


renderFloatInput : String -> String -> (String -> msg) -> Html msg
renderFloatInput label data msg =
    div []
        [ span [] [ text label ]
        , input [ onInput msg, value data, type_ "number", step ".001" ] []
        ]
