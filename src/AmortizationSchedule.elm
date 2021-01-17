module AmortizationSchedule exposing (Details, Model, Payment, Summary, computeMortgagePayment, init, view)

import Html exposing (..)
import Round
import Utils exposing (roundDollar)



-- State


init : Details -> Model
init details =
    let
        periods =
            details.salePeriod
                |> Round.round 0
                |> String.toInt
                |> Maybe.withDefault 0

        payments =
            createPayments (List.reverse (List.range 1 periods)) details
    in
    { details = details
    , payments = payments
    , summary = summarizePayments payments
    }



-- Data


type alias Model =
    { details : Details
    , payments : List Payment
    , summary : Summary
    }


type alias Details =
    { amount : Float
    , principal : Float
    , periods : Float
    , interestRate : Float
    , propertyTaxRate : Float
    , capitalGainsRate : Float
    , taxAssessmentGrowthRate : Float
    , insurancePayment : Float
    , hoaPayment : Float
    , mortgagePayment : Float
    , additionalPrincipal : Float
    , salePeriod : Float
    }


type alias Summary =
    { totalPaid : Float
    , hoaPaid : Float
    , insurancePaid : Float
    , propertyTaxPaid : Float
    , interestPaid : Float
    , principalPaid : Float
    , balance : Float
    , homeValue : Float
    }


type alias Payment =
    { homeValue : Float
    , taxAssessedValue : Float
    , principal : Float
    , mortgagePayment : Float
    , hoaPayment : Float
    , principalPayment : Float
    , interestPayment : Float
    , propertyTaxPayment : Float
    , insurancePayment : Float
    , totalPayment : Float
    , period : Int
    }



-- View


view : Model -> Html a
view model =
    div []
        [ viewDetails model
        , viewAmortizationSchedule model
        ]


viewDetails : Model -> Html msg
viewDetails model =
    let
        summary =
            model.summary

        grossProfit =
            summary.homeValue - model.details.amount

        totalNonRefundablePaid =
            computeNonRefundablePaid summary

        netProfit =
            grossProfit - totalNonRefundablePaid
    in
    div []
        [ span [] [ text "Home value: " ]
        , div [] [ summary.homeValue |> roundDollar |> text ]
        , span [] [ text "Total payments made: " ]
        , div [] [ summary.totalPaid |> roundDollar |> text ]
        , span [] [ text "Total non-refundable paid: " ]
        , div [] [ totalNonRefundablePaid |> roundDollar |> text ]
        , span [] [ text "Profit: " ]
        , div []
            [ netProfit
                |> roundDollar
                |> text
            ]
        ]


viewAmortizationSchedule : Model -> Html msg
viewAmortizationSchedule model =
    let
        payments =
            model.payments

        summary =
            model.summary
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Period" ]
                , th [] [ text "Payment" ]
                , th [] [ text "HOA" ]
                , th [] [ text "Insurance" ]
                , th [] [ text "Property Tax" ]
                , th [] [ text "Interest" ]
                , th [] [ text "Principal" ]
                , th [] [ text "Balance" ]
                , th [] [ text "Tax Assessed Value" ]
                , th [] [ text "Home Value" ]
                ]
            ]
        , payments
            |> List.reverse
            |> List.map (\payment -> viewRow payment)
            |> tbody []
        , tfoot []
            [ tr []
                [ td [] [ text "Totals" ]
                , td [] [ summary.totalPaid |> roundDollar |> text ]
                , td [] [ summary.hoaPaid |> roundDollar |> text ]
                , td [] [ summary.insurancePaid |> roundDollar |> text ]
                , td [] [ summary.propertyTaxPaid |> roundDollar |> text ]
                , td [] [ summary.interestPaid |> roundDollar |> text ]
                , td [] [ summary.principalPaid |> roundDollar |> text ]
                , td [] [ summary.balance |> roundDollar |> text ]
                , td [] [ text "-" ]
                , td [] [ summary.homeValue |> roundDollar |> text ]
                ]
            ]
        ]


viewRow : Payment -> Html msg
viewRow payment =
    tr []
        [ td [] [ text (String.fromInt payment.period) ]
        , viewRowColumn payment.totalPayment
        , viewRowColumn payment.hoaPayment
        , viewRowColumn payment.insurancePayment
        , viewRowColumn payment.propertyTaxPayment
        , viewRowColumn payment.interestPayment
        , viewRowColumn payment.principalPayment
        , viewRowColumn payment.principal
        , viewRowColumn payment.taxAssessedValue
        , viewRowColumn payment.homeValue
        ]


viewRowColumn : Float -> Html msg
viewRowColumn value =
    td [] [ value |> roundDollar |> text ]



-- Math


summarizePayments : List Payment -> Summary
summarizePayments payments =
    case payments of
        [] ->
            { totalPaid = 0.0
            , hoaPaid = 0.0
            , insurancePaid = 0.0
            , propertyTaxPaid = 0.0
            , interestPaid = 0.0
            , principalPaid = 0.0
            , balance = 0.0
            , homeValue = 0.0
            }

        p :: ps ->
            summarizePayments ps |> addPaymentToSummary p


addPaymentToSummary : Payment -> Summary -> Summary
addPaymentToSummary payment summary =
    { totalPaid = payment.totalPayment + summary.totalPaid
    , insurancePaid = payment.insurancePayment + summary.insurancePaid
    , propertyTaxPaid = payment.propertyTaxPayment + summary.propertyTaxPaid
    , interestPaid = payment.interestPayment + summary.interestPaid
    , principalPaid = payment.principalPayment + summary.principalPaid
    , hoaPaid = payment.hoaPayment + summary.hoaPaid
    , balance = payment.principal
    , homeValue = payment.homeValue
    }


computeNonRefundablePaid : Summary -> Float
computeNonRefundablePaid summary =
    summary.hoaPaid
        + summary.insurancePaid
        + summary.propertyTaxPaid
        + summary.interestPaid


createPayments : List Int -> Details -> List Payment
createPayments periods details =
    computePayments periods details


computePayments : List Int -> Details -> List Payment
computePayments periods details =
    case periods of
        [] ->
            []

        [ x ] ->
            computePayment x details Nothing :: []

        x :: xs ->
            let
                previousPayments =
                    computePayments xs details
            in
            computePayment x details (List.head previousPayments) :: previousPayments


computePayment : Int -> Details -> Maybe Payment -> Payment
computePayment period details previousPayment =
    case previousPayment of
        Just payment ->
            computePaymentFromPrevious period details payment

        Nothing ->
            computePaymentFromDetails period details


computePaymentFromPrevious : Int -> Details -> Payment -> Payment
computePaymentFromPrevious period details previousPayment =
    let
        interestPayment =
            previousPayment.principal * details.interestRate

        principalPayment =
            min
                (details.mortgagePayment - interestPayment)
                previousPayment.principal

        newPrincipal =
            previousPayment.principal - principalPayment

        homeValue =
            previousPayment.homeValue * (1 + details.capitalGainsRate)

        taxAssessedValue =
            if modBy 12 period == 0 then
                previousPayment.taxAssessedValue * (1 + details.taxAssessmentGrowthRate)

            else
                previousPayment.taxAssessedValue

        propertyTaxPayment =
            taxAssessedValue * details.propertyTaxRate

        totalPayment =
            principalPayment
                + interestPayment
                + propertyTaxPayment
                + details.insurancePayment
                + details.hoaPayment
    in
    { homeValue = homeValue
    , taxAssessedValue = taxAssessedValue
    , principal = newPrincipal
    , mortgagePayment = details.mortgagePayment
    , principalPayment = principalPayment
    , interestPayment = interestPayment
    , propertyTaxPayment = propertyTaxPayment
    , insurancePayment = details.insurancePayment
    , hoaPayment = details.hoaPayment
    , period = period
    , totalPayment = totalPayment
    }


computePaymentFromDetails : Int -> Details -> Payment
computePaymentFromDetails period details =
    let
        interestPayment =
            details.principal * details.interestRate

        principalPayment =
            min (details.mortgagePayment - interestPayment) details.principal

        newPrincipal =
            details.principal - principalPayment

        propertyTaxPayment =
            details.amount * details.propertyTaxRate

        totalPayment =
            principalPayment
                + interestPayment
                + propertyTaxPayment
                + details.insurancePayment
                + details.hoaPayment
    in
    { homeValue = details.amount
    , taxAssessedValue = details.amount
    , principal = newPrincipal
    , mortgagePayment = details.mortgagePayment
    , principalPayment = principalPayment
    , interestPayment = interestPayment
    , propertyTaxPayment = propertyTaxPayment
    , insurancePayment = details.insurancePayment
    , hoaPayment = details.hoaPayment
    , period = period
    , totalPayment = totalPayment
    }


computeMortgagePayment : Float -> Float -> Float -> Float -> Float
computeMortgagePayment principal periods rate additional =
    principal
        * ((rate * (1 + rate) ^ periods) / ((1 + rate) ^ periods - 1))
        + additional
