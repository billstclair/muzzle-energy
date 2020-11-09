-----------------------------------------------------------------
--
-- Main.elm
-- muzzle-energy.com - A muzzle energy computer
-- Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Energy.Math as Math exposing (Energy, Measurements)
import FormatNumber exposing (format)
import FormatNumber.Locales as Locales exposing (Decimals, base)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , col
        , div
        , h2
        , h3
        , img
        , input
        , option
        , p
        , pre
        , select
        , span
        , table
        , text
        , textarea
        )
import Html.Attributes
    exposing
        ( alt
        , autocomplete
        , autofocus
        , checked
        , class
        , cols
        , colspan
        , disabled
        , draggable
        , height
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseDown)
import Json.Encode as JE exposing (Value)
import PortFunnels
import Set exposing (Set)
import String.Extra as SE
import Svg exposing (Svg, svg)
import Svg.Attributes as Svga
import Svg.Button as Button exposing (Button, TriangularButtonDirection(..))
import Task
import Time exposing (Month, Posix, Zone)
import Url exposing (Url)


state =
    PortFunnels.initialState "muzzle-energy"


{-| This is used by links created by Util.toVirtualDom calls below.

It forces them to open in a new tab/window.

-}
port openWindow : Value -> Cmd msg


type alias Inputs =
    { grains : String
    , ounces : String
    , fps : String
    , inches : String
    , gauge : String
    }


emptyInputs =
    { grains = digitsFormat zeroDigits emptyMeasurements.grains
    , ounces = digitsFormat threeDigits emptyMeasurements.ounces
    , fps = digitsFormat zeroDigits emptyMeasurements.feetPerSecond
    , inches = digitsFormat threeDigits emptyMeasurements.diameterInInches
    , gauge = digitsFormat threeDigits emptyMeasurements.gauge
    }


emptyMeasurements =
    { grains = 180
    , ounces = 0
    , feetPerSecond = 2800
    , diameterInInches = 0.308
    , gauge = 0
    }
        |> Math.grainsToOunces
        |> Math.diameterInInchesToGauge


type alias Model =
    { cmdPort : Value -> Cmd Msg
    , inputs : Inputs
    , measurements : Measurements
    , energy : Energy
    }


type Page
    = HomePage
    | ColumnsPage
    | ExplorerPage


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetGrains String
    | SetOunces String
    | SetFeetPerSecond String
    | SetInches String
    | SetGauge String


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        inputs =
            emptyInputs

        measurements =
            emptyMeasurements
    in
    { cmdPort =
        PortFunnels.getCmdPort (\v -> Noop) "foo" False
    , inputs = inputs
    , measurements = measurements
    , energy = Math.computeEnergy measurements
    }
        |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ PortFunnels.subscriptions (\v -> Noop) model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        inputs =
            model.inputs
    in
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest urlRequest ->
            let
                url =
                    case urlRequest of
                        Internal u ->
                            Url.toString u

                        External u ->
                            u
            in
            model |> withCmd (openWindow <| JE.string url)

        OnUrlChange url ->
            model |> withNoCmd

        SetGrains s ->
            setInput
                (\g m ->
                    { m | grains = g }
                        |> Math.grainsToOunces
                )
                (\m i ->
                    { i | ounces = digitsFormat threeDigits m.ounces }
                )
                s
                { model | inputs = { inputs | grains = s } }
                |> withNoCmd

        SetOunces s ->
            setInput
                (\o m ->
                    { m | ounces = o }
                        |> Math.ouncesToGrains
                )
                (\m i ->
                    { i | grains = digitsFormat zeroDigits m.grains }
                )
                s
                { model | inputs = { inputs | ounces = s } }
                |> withNoCmd

        SetFeetPerSecond s ->
            setInput (\fps m -> { m | feetPerSecond = fps })
                (\m i -> i)
                s
                { model | inputs = { inputs | fps = s } }
                |> withNoCmd

        SetInches s ->
            setInput
                (\i m ->
                    { m | diameterInInches = i }
                        |> Math.diameterInInchesToGauge
                )
                (\m i ->
                    { i | gauge = digitsFormat threeDigits m.gauge }
                )
                s
                { model | inputs = { inputs | inches = s } }
                |> withNoCmd

        SetGauge s ->
            setInput
                (\g m ->
                    { m | gauge = g }
                        |> Math.diameterInGaugeToInches
                )
                (\m i ->
                    { i | inches = digitsFormat threeDigits m.diameterInInches }
                )
                s
                { model | inputs = { inputs | gauge = s } }
                |> withNoCmd


digitsFormat : Decimals -> Float -> String
digitsFormat decimals v =
    let
        locale =
            { base | decimals = decimals }
    in
    format locale v


setInput : (Float -> Measurements -> Measurements) -> (Measurements -> Inputs -> Inputs) -> String -> Model -> Model
setInput setter inputter string model =
    case String.toFloat string of
        Nothing ->
            model

        Just v ->
            let
                measurements =
                    setter v model.measurements

                inputs =
                    inputter measurements model.inputs
            in
            { model
                | inputs = inputs
                , measurements = measurements
                , energy = Math.computeEnergy measurements
            }


tr : List (Html Msg) -> Html Msg
tr elements =
    Html.tr [] elements


td : List (Html Msg) -> Html Msg
td elements =
    Html.td [ style "padding" "2px" ] elements


b : String -> Html Msg
b s =
    Html.b [] [ text s ]


numberInput : (String -> Msg) -> String -> Html Msg
numberInput wrapper v =
    input
        [ style "width" "5em"
        , onInput wrapper
        , value v
        ]
        []


numberDisplay : Decimals -> Float -> Html Msg
numberDisplay decimals v =
    let
        locale =
            { base | decimals = decimals }
    in
    input
        [ style "width" "5em"
        , value <| format locale v
        ]
        []


zeroDigits =
    Locales.Exact 0


threeDigits =
    Locales.Exact 3


view : Model -> Document Msg
view model =
    let
        inputs =
            model.inputs

        energy =
            model.energy
    in
    { title = "Muzzle Energy"
    , body =
        [ h2 [] [ text "Muzzle Energy" ]
        , table []
            [ tr
                [ td [ b "Bullet Weight (grains): " ]
                , td
                    [ numberInput SetGrains inputs.grains ]
                , td [ b "(ounces): " ]
                , td
                    [ numberInput SetOunces inputs.ounces ]
                ]
            , tr
                [ td [ b "Velocity (feet/second): " ]
                , td
                    [ numberInput SetFeetPerSecond inputs.fps ]
                ]
            , tr
                [ td [ b "Bullet diameter (inches): " ]
                , td
                    [ numberInput SetInches inputs.inches ]
                , td [ b "(gauge): " ]
                , td
                    [ numberInput SetGauge inputs.gauge ]
                ]
            , tr [ td [ text "." ] ]
            , tr
                [ td [ b "Energy: " ]
                , td [ numberDisplay zeroDigits energy.footPounds ]
                ]
            , tr
                [ td [ b "Efficacy (energy x area): " ]
                , td [ numberDisplay zeroDigits energy.efficacy ]
                ]
            ]
        , p []
            [ a [ href "https://elm-lang.org" ]
                [ text "Elm" ]
            , text " "
            , a [ href "https://github.com/billstclair/muzzle-energy/" ]
                [ text "GitHub" ]
            , text " "
            , a [ href "old/" ]
                [ text "Old" ]
            ]
        ]
    }
