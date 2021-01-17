module IncomeForm exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Utils exposing (renderIntegerInput)



-- Data


type alias Model =
    { monthlyIncome : String
    , monthlyExpenses : String
    }



-- State


type Msg
    = SetMonthlyIncome String
    | SetMonthlyExpenses String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMonthlyIncome value ->
            ( { model | monthlyIncome = value }, Cmd.none )

        SetMonthlyExpenses value ->
            ( { model | monthlyExpenses = value }, Cmd.none )


init : Model
init =
    { monthlyIncome = "10000"
    , monthlyExpenses = "5000"
    }



-- View


view : Model -> Html Msg
view model =
    div []
        [ renderIntegerInput "Income: " model.monthlyIncome SetMonthlyIncome
        , renderIntegerInput "Expenses: " model.monthlyExpenses SetMonthlyExpenses
        ]
