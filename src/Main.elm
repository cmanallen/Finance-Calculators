module Main exposing (Model, Msg, init, update, view)

import AmortizationSchedule as AS
import Browser
import Html exposing (..)
import IncomeForm as IF
import MortgageForm as MF
import Utils exposing (parseFloat)



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Data


type alias Model =
    { mortgageForm : MF.Model
    , mortgage : AS.Model
    , incomeForm : IF.Model
    }



-- State


init : () -> ( Model, Cmd Msg )
init _ =
    let
        form =
            MF.init
    in
    ( { mortgageForm = form
      , mortgage = AS.init (createMortgageDetails form)
      , incomeForm = IF.init
      }
    , Cmd.none
    )


type Msg
    = MortgageFormMsg MF.Msg
    | IncomeFormMsg IF.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MortgageFormMsg subMsg ->
            MF.update subMsg model.mortgageForm
                |> updateMortgageForm model
        IncomeFormMsg subMsg ->
            IF.update subMsg model.incomeForm
                |> updateIncomeForm model


updateIncomeForm : Model -> ( IF.Model, Cmd IF.Msg ) -> ( Model, Cmd Msg )
updateIncomeForm model ( subModel, _ ) =
    ( { model | incomeForm = subModel }, Cmd.none )


updateMortgageForm : Model -> ( MF.Model, Cmd MF.Msg ) -> ( Model, Cmd Msg )
updateMortgageForm model ( subModel, _ ) =
    ( { model | mortgageForm = subModel, mortgage = AS.init (createMortgageDetails subModel) }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ MF.view model.mortgageForm |> viewMortgageForm
        , IF.view model.incomeForm |> viewIncomeForm
        , AS.view model.mortgage
        ]


viewIncomeForm : Html IF.Msg -> Html Msg
viewIncomeForm msg =
    Html.map IncomeFormMsg msg


viewMortgageForm : Html MF.Msg -> Html Msg
viewMortgageForm msg =
    Html.map MortgageFormMsg msg



-- Helpers


createMortgageDetails : MF.Model -> AS.Details
createMortgageDetails model =
    let
        downPayment =
            parseFloat model.downPayment

        principal =
            parseFloat model.principal - downPayment

        periods =
            parseFloat model.loanTerm * 12

        rate =
            parseFloat model.interestRate / 1200

        propertyTaxRate =
            parseFloat model.propertyTaxRate / 1200

        capitalGainsRate =
            parseFloat model.capitalGainsRate / 1200

        taxAssessmentGrowthRate =
            parseFloat model.taxAssessmentGrowthRate / 100

        additionalPrincipal =
            parseFloat model.additionalPrincipal

        mortgagePayment =
            AS.computeMortgagePayment principal periods rate additionalPrincipal
    in
    { amount = parseFloat model.principal
    , principal = principal
    , periods = periods
    , interestRate = rate
    , propertyTaxRate = propertyTaxRate
    , capitalGainsRate = capitalGainsRate
    , insurancePayment = parseFloat model.insurancePayment
    , mortgagePayment = mortgagePayment
    , additionalPrincipal = additionalPrincipal
    , salePeriod = parseFloat model.salePeriod
    , hoaPayment = parseFloat model.hoaPayment
    , taxAssessmentGrowthRate = taxAssessmentGrowthRate
    }
