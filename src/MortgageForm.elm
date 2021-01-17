module MortgageForm exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Utils exposing (renderFloatInput, renderIntegerInput)



-- Data


type alias Model =
    { principal : String
    , downPayment : String
    , loanTerm : String
    , insurancePayment : String
    , hoaPayment : String
    , interestRate : String
    , propertyTaxRate : String
    , capitalGainsRate : String
    , salePeriod : String
    , additionalPrincipal : String
    , taxAssessmentGrowthRate : String
    }



-- State


type Msg
    = SetPrincipal String
    | SetDownPayment String
    | SetLoanTerm String
    | SetInsurancePayment String
    | SetHOAPayment String
    | SetInterestRate String
    | SetPropertyTaxRate String
    | SetCapitalGainsRate String
    | SetSalePeriod String
    | SetAdditionalPrincipal String
    | SetTaxAssessmentGrowthRate String


init : Model
init =
    { principal = "800000"
    , downPayment = "160000"
    , loanTerm = "30"
    , insurancePayment = "280"
    , interestRate = "3.288"
    , propertyTaxRate = "1.12"
    , capitalGainsRate = "3.5"
    , hoaPayment = "0"
    , salePeriod = "360"
    , additionalPrincipal = "0"
    , taxAssessmentGrowthRate = "0.5"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPrincipal value ->
            ( { model | principal = value }, Cmd.none )

        SetDownPayment value ->
            ( { model | downPayment = value }, Cmd.none )

        SetLoanTerm value ->
            ( { model | loanTerm = value }, Cmd.none )

        SetInsurancePayment value ->
            ( { model | insurancePayment = value }, Cmd.none )

        SetHOAPayment value ->
            ( { model | hoaPayment = value }, Cmd.none )

        SetInterestRate value ->
            ( { model | interestRate = value }, Cmd.none )

        SetPropertyTaxRate value ->
            ( { model | propertyTaxRate = value }, Cmd.none )

        SetCapitalGainsRate value ->
            ( { model | capitalGainsRate = value }, Cmd.none )

        SetSalePeriod value ->
            ( { model | salePeriod = value }, Cmd.none )

        SetAdditionalPrincipal value ->
            ( { model | additionalPrincipal = value }, Cmd.none )

        SetTaxAssessmentGrowthRate value ->
            ( { model | taxAssessmentGrowthRate = value }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ renderIntegerInput "Principal "
            model.principal
            SetPrincipal
        , renderIntegerInput "Down Payment "
            model.downPayment
            SetDownPayment
        , renderIntegerInput "Additional Principal "
            model.additionalPrincipal
            SetAdditionalPrincipal
        , renderIntegerInput "Loan Term "
            model.loanTerm
            SetLoanTerm
        , renderIntegerInput "Sale Period "
            model.salePeriod
            SetSalePeriod
        , renderIntegerInput "Insurance Payment (monthly) "
            model.insurancePayment
            SetInsurancePayment
        , renderIntegerInput "HOA Payment (monthly) "
            model.hoaPayment
            SetHOAPayment
        , renderFloatInput "Interest Rate %"
            model.interestRate
            SetInterestRate
        , renderFloatInput "Property Tax Rate %"
            model.propertyTaxRate
            SetPropertyTaxRate
        , renderFloatInput "Tax Assessed Value Growth Rate %"
            model.taxAssessmentGrowthRate
            SetTaxAssessmentGrowthRate
        , renderFloatInput "Capital Gains Rate %"
            model.capitalGainsRate
            SetCapitalGainsRate
        ]
