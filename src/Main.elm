-----------------------------------------------------------------
--
-- Main.elm
-- muzzle-energy.com - A muzzle energy computer
-- Copyright (c) 2020-2025 Bill St. Clair <billstclair@gmail.com>
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
import Dialog
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
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import PortFunnel.LocalStorage as LocalStorage
    exposing
        ( Message
        , Response(..)
        )
import PortFunnels exposing (FunnelDict, Handler(..))
import Set exposing (Set)
import String.Extra as SE
import Svg exposing (Svg, svg)
import Svg.Attributes as Svga
import Svg.Button as Button exposing (Button, TriangularButtonDirection(..))
import Task
import Time exposing (Month, Posix, Zone)
import Url exposing (Url)


funnelState =
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


type alias CaliberInputs =
    Dict Int String


measurementsToInputs : Measurements -> Inputs
measurementsToInputs measurements =
    { grains = digitsFormat zeroDigits measurements.grains
    , ounces = digitsFormat threeDigits measurements.ounces
    , fps = digitsFormat zeroDigits measurements.feetPerSecond
    , inches = digitsFormat threeDigits measurements.diameterInInches
    , gauge = digitsFormat threeDigits measurements.gauge
    }


emptyMeasurements : Measurements
emptyMeasurements =
    { grains = 180
    , ounces = 0
    , feetPerSecond = 2800
    , diameterInInches = 0.308
    , gauge = 0
    }
        |> Math.grainsToOunces
        |> Math.diameterInInchesToGauge


emptySample : Sample
emptySample =
    { name = ""
    , weapon = Shotgun
    , sort = -1
    , unit = Yards
    , distance = 0
    , measurements = emptyMeasurements
    }


type Started
    = NotStarted
    | StartedReadingModel
    | Started


type Unit
    = Feet
    | Yards


encodeWeapon : Weapon -> Value
encodeWeapon weapon =
    JE.string <| weaponToString weapon


weaponDecoder : Decoder Weapon
weaponDecoder =
    JD.string
        |> JD.andThen
            (\string -> JD.succeed <| stringToWeapon string)


encodeUnit : Unit -> Value
encodeUnit unit =
    JE.string <| unitToString unit


unitDecoder : Decoder Unit
unitDecoder =
    JD.string
        |> JD.andThen
            (\s -> JD.succeed <| stringToUnit s)


unitToString : Unit -> String
unitToString unit =
    case unit of
        Feet ->
            "feet"

        Yards ->
            "yards"


stringToUnit : String -> Unit
stringToUnit string =
    if string == "feet" then
        Feet

    else
        Yards


type Weapon
    = Rifle
    | Handgun
    | Shotgun


weaponToString : Weapon -> String
weaponToString weapon =
    case weapon of
        Rifle ->
            "Rifle"

        Handgun ->
            "Handgun"

        Shotgun ->
            "Shotgun"


weaponToInt : Weapon -> Int
weaponToInt weapon =
    case weapon of
        Rifle ->
            0

        Handgun ->
            1

        Shotgun ->
            2


stringToWeapon : String -> Weapon
stringToWeapon string =
    if string == "Handgun" then
        Handgun

    else if string == "Shotgun" then
        Shotgun

    else
        Rifle


type alias Sample =
    { name : String
    , weapon : Weapon
    , sort : Float
    , unit : Unit
    , distance : Int
    , measurements : Measurements
    }


type alias SampleDisplay =
    { name : String
    , weapon : Weapon
    , sort : Float
    , unit : Unit
    , distance : Int
    }


sampleToDisplay : Sample -> SampleDisplay
sampleToDisplay { name, weapon, sort, unit, distance } =
    SampleDisplay name weapon sort unit distance


type alias SampleKey =
    ( ( Int, Float, String ), ( String, Int ) )


encodeSampleDisplay : SampleDisplay -> Value
encodeSampleDisplay { name, weapon, sort, unit, distance } =
    JE.object
        [ ( "name", JE.string name )
        , ( "weapon", encodeWeapon weapon )
        , ( "sort", JE.float sort )
        , ( "unit", encodeUnit unit )
        , ( "distance", JE.int distance )
        ]


sampleDisplayDecoder : Decoder SampleDisplay
sampleDisplayDecoder =
    JD.succeed SampleDisplay
        |> required "name" JD.string
        |> required "weapon" weaponDecoder
        |> required "sort" JD.float
        |> required "unit" unitDecoder
        |> required "distance" JD.int


sampleToKey : Sample -> SampleKey
sampleToKey sample =
    sampleDisplayToKey <| sampleToDisplay sample


sampleDisplayToKey : SampleDisplay -> SampleKey
sampleDisplayToKey { name, weapon, sort, unit, distance } =
    ( ( weaponToInt weapon, sort, name ), ( unitToString unit, distance ) )


type alias SampleDict =
    Dict SampleKey Sample


sn =
    { s223 = "223 Remington (5.56 NATO)"
    , s762 = "7.62x39 (AK-47)"
    , s308 = "308 Winchester (7.72 NATO)"
    , s3006 = "30-06 Springfield"
    , s444 = "444 Marlin"
    , s380 = "380 Auto"
    , s9mm = "9mm Luger"
    , s357 = "357 Magnum"
    , s40sw = "40 S&W"
    , s45acp = "45 Auto (ACP)"
    , s44mag = "44 Magnum"
    , s12ga = "12 Gauge Slug"
    }


ms : Float -> Float -> Float -> Measurements
ms grains feetPerSecond diameterInInches =
    { grains = grains
    , feetPerSecond = feetPerSecond
    , diameterInInches = diameterInInches
    , ounces = 0
    , gauge = 0
    }
        |> Math.grainsToOunces
        |> Math.diameterInInchesToGauge


oneCaliberSamples : String -> Weapon -> Float -> Unit -> Float -> Float -> List ( Int, Float ) -> List Sample
oneCaliberSamples name weapon sort unit grains diameter distanceAndFPSs =
    List.map
        (\( distance, fps ) ->
            Sample name weapon sort unit distance (ms grains fps diameter)
        )
        distanceAndFPSs


sn308Samples : List Sample
sn308Samples =
    oneCaliberSamples sn.s308
        Rifle
        2
        Yards
        150
        0.308
        [ ( 0, 2820 )
        , ( 100, 2533 )
        , ( 200, 2263 )
        , ( 300, 2009 )
        ]


initialSampleDict : SampleDict
initialSampleDict =
    [ oneCaliberSamples sn.s223
        Rifle
        0
        Yards
        55
        0.224
        [ ( 0, 3240 )
        , ( 100, 2773 )
        , ( 200, 2352 )
        , ( 300, 1969 )
        ]
    , oneCaliberSamples sn.s762
        Rifle
        1
        Yards
        125
        0.309
        [ ( 0, 2365 )
        , ( 100, 2062 )
        , ( 200, 1783 )
        , ( 300, 1533 )
        ]
    , sn308Samples
    , oneCaliberSamples sn.s3006
        Rifle
        3
        Yards
        180
        0.308
        [ ( 0, 2700 )
        , ( 100, 2485 )
        , ( 200, 2280 )
        , ( 300, 2084 )
        ]
    , oneCaliberSamples sn.s444
        Rifle
        4
        Yards
        240
        0.43
        [ ( 0, 2350 )
        , ( 100, 1815 )
        , ( 200, 1377 )
        , ( 300, 2084 )
        ]
    , oneCaliberSamples sn.s380
        Handgun
        0
        Feet
        90
        0.357
        [ ( 0, 1000 )
        , ( 50, 910 )
        , ( 100, 841 )
        ]
    , oneCaliberSamples sn.s9mm
        Handgun
        1
        Feet
        124
        0.355
        [ ( 0, 1180 )
        , ( 50, 1089 )
        , ( 100, 1021 )
        ]
    , oneCaliberSamples sn.s357
        Handgun
        2
        Feet
        158
        0.358
        [ ( 0, 1235 )
        , ( 50, 1104 )
        , ( 100, 1015 )
        ]
    , oneCaliberSamples sn.s40sw
        Handgun
        3
        Feet
        165
        0.402
        [ ( 0, 1150 )
        , ( 50, 1040 )
        , ( 100, 964 )
        ]
    , oneCaliberSamples sn.s45acp
        Handgun
        4
        Feet
        230
        0.451
        [ ( 0, 875 )
        , ( 50, 833 )
        , ( 100, 795 )
        ]
    , oneCaliberSamples sn.s44mag
        Handgun
        5
        Feet
        240
        0.43
        [ ( 0, 1180 )
        , ( 50, 1081 )
        , ( 100, 1010 )
        ]
    , oneCaliberSamples sn.s12ga
        Shotgun
        0
        Yards
        437
        0.729
        [ ( 0, 1680 )
        , ( 50, 1285 )
        , ( 100, 1045 )
        ]
    ]
        |> List.concat
        |> List.map (\s -> ( sampleToKey s, s ))
        |> Dict.fromList


type alias Model =
    { cmdPort : Value -> Cmd Msg
    , dialog : Dialog
    , inputs : Inputs
    , measurements : Measurements
    , energy : Energy
    , started : Started
    , samples : SampleDict
    , sample : Maybe SampleDisplay
    , editSamples : SampleDict
    , editSample : Maybe SampleDisplay
    , editInputs : Inputs
    , editNewInputs : NewInputs
    , editSortInput : String
    , caliberInputs : CaliberInputs
    }


type Dialog
    = NoDialog
    | EditDialog


type SortDirection
    = SortUp
    | SortDown


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetGrains String
    | SetOunces String
    | SetFeetPerSecond String
    | SetInches String
    | SetGauge String
    | SetEditGrains String
    | SetEditOunces String
    | SetEditFeetPerSecond String
    | SetEditInches String
    | SetEditGauge String
    | DeleteEditSample Sample
    | DeleteEditDistance Sample
    | SetEditVelocity Sample String
    | MoveEditSort Sample SortDirection
    | SetEditNewDistance Sample String
    | AddEditDistance Sample
    | SetEditSample Sample
    | SetSample Sample
    | SetEditNewShow Bool
    | SetEditNewName String
    | SetEditNewWeapon String
    | SetEditNewUnit String
    | SetEditNewGrains String
    | SetEditNewOunces String
    | SetEditNewInches String
    | SetEditNewGauge String
    | SetEditNewMuzzleFps String
    | NewEditCaliber
    | Process Value
    | SetDialog Dialog
    | DismissDialog
    | CommitEditDialog
    | RestoreDefaultSamples


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
        measurements =
            emptyMeasurements

        inputs =
            measurementsToInputs measurements

        model =
            { cmdPort =
                PortFunnels.getCmdPort (\v -> Noop) "foo" False
            , dialog = NoDialog
            , inputs = inputs
            , measurements = measurements
            , energy = Math.computeEnergy measurements
            , started = NotStarted
            , samples = initialSampleDict
            , sample = Nothing
            , editSamples = Dict.empty
            , editSample = Nothing
            , editInputs = measurementsToInputs emptyMeasurements
            , editNewInputs = initialNewInputs
            , editSortInput = "0"
            , caliberInputs = Dict.empty
            }

        ( mdl, _ ) =
            case List.head sn308Samples of
                Nothing ->
                    ( model, Cmd.none )

                Just sample ->
                    update (SetSample sample) model
    in
    mdl |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ PortFunnels.subscriptions Process model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( mdl, cmd ) =
            updateInternal msg model

        savedModel =
            modelToSaved model

        savedMdl =
            modelToSaved mdl
    in
    mdl
        |> withCmds
            [ cmd
            , if savedModel == savedMdl then
                Cmd.none

              else
                put pk.model (Just <| encodeSavedModel savedMdl)
            , if model.samples == mdl.samples then
                Cmd.none

              else
                putSamples mdl.samples
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
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
                { model
                    | inputs = { inputs | grains = s }
                    , sample = Nothing
                }
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
                { model
                    | inputs = { inputs | ounces = s }
                    , sample = Nothing
                }
                |> withNoCmd

        SetFeetPerSecond s ->
            setInput (\fps m -> { m | feetPerSecond = fps })
                (\m i -> i)
                s
                { model
                    | inputs = { inputs | fps = s }
                    , sample = Nothing
                }
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
                { model
                    | inputs = { inputs | inches = s }
                    , sample = Nothing
                }
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
                { model
                    | inputs = { inputs | gauge = s }
                    , sample = Nothing
                }
                |> withNoCmd

        SetEditGrains string ->
            updateMeasurementsInput True
                (\grains measurements ->
                    { measurements | grains = grains }
                        |> Math.grainsToOunces
                )
                (\measurements inps ->
                    case measurements of
                        Nothing ->
                            { inps | grains = string }

                        Just m ->
                            { inps
                                | grains = string
                                , ounces =
                                    digitsFormat threeDigits m.ounces
                            }
                )
                string
                model
                |> withNoCmd

        SetEditOunces string ->
            updateMeasurementsInput True
                (\ounces measurements ->
                    { measurements | ounces = ounces }
                        |> Math.ouncesToGrains
                )
                (\measurements inps ->
                    case measurements of
                        Nothing ->
                            { inps | ounces = string }

                        Just m ->
                            { inps
                                | ounces = string
                                , grains =
                                    digitsFormat zeroDigits m.grains
                            }
                )
                string
                model
                |> withNoCmd

        SetEditFeetPerSecond string ->
            updateMeasurementsInput False
                (\fps measurements ->
                    { measurements | feetPerSecond = fps }
                )
                (\measurements inps ->
                    { inps | fps = string }
                )
                string
                model
                |> withNoCmd

        SetEditInches string ->
            updateMeasurementsInput False
                (\inches measurements ->
                    { measurements | diameterInInches = inches }
                        |> Math.diameterInInchesToGauge
                )
                (\measurements inps ->
                    case measurements of
                        Nothing ->
                            { inps | inches = string }

                        Just m ->
                            { inps
                                | inches = string
                                , gauge =
                                    digitsFormat threeDigits m.gauge
                            }
                )
                string
                model
                |> withNoCmd

        SetEditGauge string ->
            updateMeasurementsInput False
                (\gauge measurements ->
                    { measurements | gauge = gauge }
                        |> Math.diameterInGaugeToInches
                )
                (\measurements inps ->
                    case measurements of
                        Nothing ->
                            { inps | gauge = string }

                        Just m ->
                            { inps
                                | gauge = string
                                , inches =
                                    digitsFormat threeDigits m.diameterInInches
                            }
                )
                string
                model
                |> withNoCmd

        DeleteEditSample sample ->
            let
                display =
                    sampleToDisplay { sample | sort = 0, distance = 0 }

                filter =
                    \_ v ->
                        display /= sampleToDisplay { v | sort = 0, distance = 0 }
            in
            { model
                | editSamples = Dict.filter filter model.editSamples
                , editSample =
                    case model.editSample of
                        Nothing ->
                            Nothing

                        Just editSample ->
                            if display == { editSample | sort = 0, distance = 0 } then
                                Nothing

                            else
                                model.editSample
            }
                |> withNoCmd

        DeleteEditDistance sample ->
            let
                key =
                    sampleToKey sample
            in
            { model | editSamples = Dict.remove key model.editSamples }
                |> withNoCmd

        SetEditVelocity sample string ->
            let
                mdl =
                    updateMeasurementsInput True
                        (\fps measurements ->
                            { measurements | feetPerSecond = fps }
                        )
                        (\measurements inps ->
                            inps
                        )
                        string
                        { model | editSample = Just <| sampleToDisplay sample }
            in
            { mdl
                | caliberInputs =
                    Dict.insert sample.distance string model.caliberInputs
            }
                |> withNoCmd

        MoveEditSort sample direction ->
            let
                pred =
                    case direction of
                        SortUp ->
                            (>)

                        SortDown ->
                            (<)

                sorts =
                    dictToSamples model.editSamples
                        |> List.filter (.weapon >> (==) sample.weapon)
                        |> List.map .sort
                        |> LE.unique
                        |> (if direction == SortUp then
                                List.reverse

                            else
                                identity
                           )

                old =
                    sample.sort
            in
            case LE.find (pred old) sorts of
                Nothing ->
                    model |> withNoCmd

                Just new ->
                    let
                        mdl =
                            updateEditSamples False
                                False
                                (\samp ->
                                    if samp.weapon /= sample.weapon then
                                        samp

                                    else
                                        { samp
                                            | sort =
                                                if samp.sort == old then
                                                    new

                                                else if samp.sort == new then
                                                    old

                                                else
                                                    samp.sort
                                        }
                                )
                                (\measurements inps -> inps)
                                model

                        editSample =
                            case model.editSample of
                                Nothing ->
                                    Nothing

                                Just es ->
                                    Just { es | sort = new }
                    in
                    { mdl | editSample = editSample }
                        |> withNoCmd

        SetEditNewDistance sample string ->
            -- TODO
            let
                editNewInputs =
                    model.editNewInputs
            in
            { model
                | editNewInputs =
                    { editNewInputs
                        | newDistance = string
                    }
            }
                |> withNoCmd

        AddEditDistance sample ->
            addEditDistance model
                |> withNoCmd

        SetEditSample sample ->
            let
                editSample =
                    sampleToDisplay sample
            in
            { model
                | editSample = Just editSample
                , editInputs = measurementsToInputs sample.measurements
                , caliberInputs = computeCaliberInputs editSample model.editSamples
            }
                |> withNoCmd

        SetSample sample ->
            let
                measurements =
                    sample.measurements
            in
            { model
                | measurements = measurements
                , inputs = measurementsToInputs measurements
                , energy = Math.computeEnergy measurements
                , sample = Just <| sampleToDisplay sample
            }
                |> withNoCmd

        SetEditNewShow show ->
            setEditNewInputs (\ni -> { ni | show = show }) model
                |> withNoCmd

        SetEditNewName string ->
            setEditNewInputs (\ni -> { ni | name = string }) model
                |> withNoCmd

        SetEditNewWeapon string ->
            setEditNewInputs (\ni -> { ni | weapon = stringToWeapon string }) model
                |> withNoCmd

        SetEditNewUnit string ->
            setEditNewInputs (\ni -> { ni | unit = stringToUnit string }) model
                |> withNoCmd

        SetEditNewGrains string ->
            let
                mdl =
                    case String.toFloat string of
                        Nothing ->
                            setEditNewInputs (\ni -> { ni | grains = string }) model

                        Just grains ->
                            let
                                measurements =
                                    { emptyMeasurements | grains = grains }
                                        |> Math.grainsToOunces

                                ounces =
                                    digitsFormat threeDigits measurements.ounces
                            in
                            setEditNewInputs
                                (\ni ->
                                    { ni
                                        | grains = string
                                        , ounces = ounces
                                    }
                                )
                                model
            in
            mdl |> withNoCmd

        SetEditNewOunces string ->
            let
                mdl =
                    case String.toFloat string of
                        Nothing ->
                            setEditNewInputs (\ni -> { ni | ounces = string }) model

                        Just ounces ->
                            let
                                measurements =
                                    { emptyMeasurements | ounces = ounces }
                                        |> Math.ouncesToGrains

                                grains =
                                    digitsFormat zeroDigits measurements.grains
                            in
                            setEditNewInputs
                                (\ni ->
                                    { ni
                                        | grains = grains
                                        , ounces = string
                                    }
                                )
                                model
            in
            mdl |> withNoCmd

        SetEditNewInches string ->
            let
                mdl =
                    case String.toFloat string of
                        Nothing ->
                            setEditNewInputs (\ni -> { ni | inches = string }) model

                        Just inches ->
                            let
                                measurements =
                                    { emptyMeasurements | diameterInInches = inches }
                                        |> Math.diameterInInchesToGauge

                                gauge =
                                    digitsFormat threeDigits measurements.gauge
                            in
                            setEditNewInputs
                                (\ni ->
                                    { ni
                                        | inches = string
                                        , gauge = gauge
                                    }
                                )
                                model
            in
            mdl |> withNoCmd

        SetEditNewGauge string ->
            let
                mdl =
                    case String.toFloat string of
                        Nothing ->
                            setEditNewInputs (\ni -> { ni | gauge = string }) model

                        Just gauge ->
                            let
                                measurements =
                                    { emptyMeasurements | gauge = gauge }
                                        |> Math.diameterInGaugeToInches

                                inches =
                                    digitsFormat threeDigits measurements.diameterInInches
                            in
                            setEditNewInputs
                                (\ni ->
                                    { ni
                                        | inches = inches
                                        , gauge = string
                                    }
                                )
                                model
            in
            mdl |> withNoCmd

        SetEditNewMuzzleFps string ->
            setEditNewInputs (\ni -> { ni | muzzleFps = string }) model
                |> withNoCmd

        NewEditCaliber ->
            newEditCaliber model
                |> withNoCmd

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    -- no state change for LocalStorage
                    initialFunnelState
                    model
            of
                Err error ->
                    let
                        err =
                            Debug.log "Process error" error
                    in
                    model |> withNoCmd

                Ok ( mdl, cmd ) ->
                    let
                        mdl2 =
                            if mdl.measurements == model.measurements then
                                mdl

                            else
                                { mdl
                                    | inputs = measurementsToInputs mdl.measurements
                                    , energy = Math.computeEnergy mdl.measurements
                                }
                    in
                    mdl2 |> withCmd cmd

        SetDialog dialog ->
            let
                mdl =
                    case dialog of
                        EditDialog ->
                            { model
                                | editSamples = model.samples
                                , editSample = model.sample
                                , editInputs = model.inputs
                                , caliberInputs =
                                    case model.sample of
                                        Nothing ->
                                            model.caliberInputs

                                        Just samp ->
                                            computeCaliberInputs samp model.samples
                            }

                        _ ->
                            model
            in
            { mdl | dialog = dialog }
                |> withNoCmd

        DismissDialog ->
            { model | dialog = NoDialog }
                |> withNoCmd

        CommitEditDialog ->
            { model
                | dialog = NoDialog
                , samples = model.editSamples
                , sample = model.editSample
                , inputs = model.editInputs
            }
                |> withNoCmd

        RestoreDefaultSamples ->
            { model
                | editSamples = initialSampleDict
                , editSample = Nothing
            }
                |> withNoCmd


addEditDistance : Model -> Model
addEditDistance model =
    let
        editNewInputs =
            model.editNewInputs
    in
    case model.editSample of
        Nothing ->
            model

        Just editSample ->
            let
                { name, weapon, sort, unit } =
                    editSample
            in
            case String.toInt editNewInputs.newDistance of
                Nothing ->
                    model

                Just distance ->
                    let
                        key =
                            sampleDisplayToKey { editSample | distance = 0 }
                    in
                    case Dict.get key model.editSamples of
                        Nothing ->
                            model

                        Just { measurements } ->
                            let
                                sample =
                                    { name = name
                                    , weapon = weapon
                                    , sort = sort
                                    , unit = unit
                                    , distance = distance
                                    , measurements =
                                        { measurements | feetPerSecond = 0 }
                                    }
                            in
                            { model
                                | editNewInputs =
                                    { editNewInputs | newDistance = "" }
                                , editSamples =
                                    Dict.insert (sampleToKey sample)
                                        sample
                                        model.editSamples
                            }


newEditCaliber : Model -> Model
newEditCaliber model =
    let
        inputs =
            model.editNewInputs

        editSamples =
            model.editSamples

        { name, weapon, unit, grains, inches, muzzleFps } =
            inputs

        grainsInchesM =
            case String.toFloat grains of
                Nothing ->
                    Nothing

                Just g ->
                    case String.toFloat inches of
                        Nothing ->
                            Nothing

                        Just i ->
                            case String.toFloat muzzleFps of
                                Nothing ->
                                    Nothing

                                Just m ->
                                    Just ( g, i, m )
    in
    case grainsInchesM of
        Nothing ->
            model

        Just ( grainsF, inchesF, muzzleFpsF ) ->
            let
                sample =
                    { name = name
                    , weapon = weapon
                    , sort =
                        Dict.values editSamples
                            |> List.map .sort
                            |> List.foldl max 0
                            |> (+) 1
                    , unit = unit
                    , distance = 0
                    , measurements =
                        { emptyMeasurements
                            | grains = grainsF
                            , feetPerSecond = muzzleFpsF
                            , diameterInInches = inchesF
                        }
                            |> Math.grainsToOunces
                            |> Math.diameterInInchesToGauge
                    }

                key =
                    sampleToKey sample

                newEditSamples =
                    case Dict.get key editSamples of
                        Nothing ->
                            Dict.insert key sample editSamples

                        _ ->
                            editSamples
            in
            { model
                | editSamples = newEditSamples
                , editNewInputs = { inputs | name = "" }
            }


setEditNewInputs : (NewInputs -> NewInputs) -> Model -> Model
setEditNewInputs setter model =
    let
        editNewInputs =
            model.editNewInputs
    in
    { model
        | editNewInputs =
            setter editNewInputs
    }


computeCaliberInputs : SampleDisplay -> SampleDict -> CaliberInputs
computeCaliberInputs sample dict =
    let
        { name, weapon, unit } =
            sample

        samples =
            dictToSamples dict

        samps =
            List.filter
                (\s ->
                    (name == s.name)
                        && (weapon == s.weapon)
                        && (unit == s.unit)
                )
                samples
    in
    List.map
        (\s ->
            ( s.distance
            , digitsFormat zeroDigits s.measurements.feetPerSecond
            )
        )
        samps
        |> Dict.fromList


updateMeasurementsInput : Bool -> (Float -> Measurements -> Measurements) -> (Maybe Measurements -> Inputs -> Inputs) -> String -> Model -> Model
updateMeasurementsInput matchDistance updater inputsUpdater string model =
    case String.toFloat string of
        Nothing ->
            { model
                | editInputs =
                    inputsUpdater Nothing model.inputs
            }

        Just float ->
            updateEditSamples matchDistance
                True
                (\sample ->
                    { sample
                        | measurements =
                            updater float sample.measurements
                    }
                )
                inputsUpdater
                model


updateEditSamples : Bool -> Bool -> (Sample -> Sample) -> (Maybe Measurements -> Inputs -> Inputs) -> Model -> Model
updateEditSamples matchDistance matchName updater inputsUpdater model =
    case model.editSample of
        Nothing ->
            model

        Just sampleDisplay ->
            let
                dist =
                    { sampleDisplay
                        | sort = 0
                        , distance =
                            if matchDistance then
                                sampleDisplay.distance

                            else
                                -1
                        , name =
                            if matchName then
                                sampleDisplay.name

                            else
                                ""
                    }

                newSamples =
                    LE.updateIf (sampleMatches matchDistance matchName dist)
                        (\( key, sample ) ->
                            let
                                newSample =
                                    updater sample

                                newKey =
                                    sampleToKey newSample
                            in
                            ( newKey, newSample )
                        )
                    <|
                        Dict.toList model.editSamples

                exactDist =
                    { sampleDisplay | sort = 0 }

                editInputs =
                    case LE.find (sampleMatches True True exactDist) newSamples of
                        Nothing ->
                            model.editInputs

                        Just ( _, es ) ->
                            inputsUpdater (Just es.measurements) model.editInputs
            in
            { model
                | editSamples = Dict.fromList newSamples
                , editInputs = editInputs
            }


sampleMatches : Bool -> Bool -> SampleDisplay -> ( key, Sample ) -> Bool
sampleMatches matchDistance matchName display ( _, sample ) =
    display
        == sampleToDisplay
            { sample
                | sort = 0
                , distance =
                    if matchDistance then
                        sample.distance

                    else
                        -1
                , name =
                    if matchName then
                        sample.name

                    else
                        ""
            }


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict [ LocalStorageHandler storageHandler ] getCmdPort


{-| Get the output port. Simulation not supported (`False` below).
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName False


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if
                        LocalStorage.isLoaded state.storage
                            && (model.started == NotStarted)
                    then
                        StartedReadingModel

                    else
                        model.started
            }

        cmd =
            if
                (mdl.started == StartedReadingModel)
                    && (model.started == NotStarted)
            then
                Cmd.batch [ get pk.model, get pk.calibers ]

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            let
                mdl2 =
                    { mdl | started = Started }
            in
            case value of
                Nothing ->
                    mdl2 |> withCmd cmd

                Just v ->
                    if key == pk.model then
                        case JD.decodeValue savedModelDecoder v of
                            Err err ->
                                let
                                    e =
                                        Debug.log "model decoding error" err
                                in
                                mdl2 |> withCmd cmd

                            Ok savedModel ->
                                savedToModel savedModel mdl2 |> withCmd cmd

                    else if key == pk.calibers then
                        case JD.decodeValue (JD.list caliberDecoder) v of
                            Err err ->
                                let
                                    e =
                                        Debug.log "calibers decoding error" err
                                in
                                mdl2 |> withCmd cmd

                            Ok calibers ->
                                { model
                                    | samples =
                                        ungroupCalibers calibers
                                            |> samplesToDict
                                }
                                    |> withCmd cmd

                    else
                        mdl2 |> withCmd cmd

        LocalStorage.ListKeysResponse { label, keys } ->
            mdl
                |> withCmd cmd

        _ ->
            mdl |> withCmd cmd


type alias SavedModel =
    { measurements : Measurements
    , sample : Maybe SampleDisplay
    }


savedToModel : SavedModel -> Model -> Model
savedToModel saved model =
    { model
        | measurements = saved.measurements
        , sample = saved.sample
    }


modelToSaved : Model -> SavedModel
modelToSaved model =
    { measurements = model.measurements
    , sample = model.sample
    }


encodeSavedModel : SavedModel -> Value
encodeSavedModel { measurements, sample } =
    JE.object
        [ ( "measurements", Math.encodeMeasurements measurements )
        , ( "sample"
          , case sample of
                Nothing ->
                    JE.null

                Just s ->
                    encodeSampleDisplay s
          )
        ]


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> required "measurements" Math.measurementsDecoder
        |> optional "sample" (JD.nullable sampleDisplayDecoder) Nothing


type alias Chronograph =
    { distance : Int
    , feetPerSecond : Float
    }


encodeChronograph : Chronograph -> Value
encodeChronograph { distance, feetPerSecond } =
    JE.object
        [ ( "distance", JE.int distance )
        , ( "feetPerSecond", JE.float feetPerSecond )
        ]


chronographDecoder : Decoder Chronograph
chronographDecoder =
    JD.succeed Chronograph
        |> required "distance" JD.int
        |> required "feetPerSecond" JD.float


type alias Caliber =
    { name : String
    , weapon : Weapon
    , sort : Float
    , unit : Unit
    , grains : Float
    , diameterInInches : Float
    , chronographs : List Chronograph
    }


encodeCaliber : Caliber -> Value
encodeCaliber { name, weapon, sort, unit, grains, diameterInInches, chronographs } =
    JE.object
        [ ( "name", JE.string name )
        , ( "weapon", encodeWeapon weapon )
        , ( "sort", JE.float sort )
        , ( "unit", encodeUnit unit )
        , ( "grains", JE.float grains )
        , ( "diameterInInches", JE.float diameterInInches )
        , ( "chronographs", JE.list encodeChronograph chronographs )
        ]


caliberDecoder : Decoder Caliber
caliberDecoder =
    JD.succeed Caliber
        |> required "name" JD.string
        |> required "weapon" weaponDecoder
        |> required "sort" JD.float
        |> required "unit" unitDecoder
        |> required "grains" JD.float
        |> required "diameterInInches" JD.float
        |> required "chronographs" (JD.list chronographDecoder)


ungroupCalibers : List Caliber -> List Sample
ungroupCalibers calibers =
    let
        ungroup : Caliber -> List Sample
        ungroup caliber =
            let
                sample =
                    { name = caliber.name
                    , weapon = caliber.weapon
                    , sort = caliber.sort
                    , unit = caliber.unit
                    , distance = 0
                    , measurements = emptyMeasurements
                    }

                measurements =
                    { emptyMeasurements
                        | grains = caliber.grains
                        , diameterInInches = caliber.diameterInInches
                    }
                        |> Math.grainsToOunces
                        |> Math.diameterInInchesToGauge

                chronographToSample chronograph =
                    { sample
                        | distance =
                            chronograph.distance
                        , measurements =
                            { measurements
                                | feetPerSecond =
                                    chronograph.feetPerSecond
                            }
                    }
            in
            List.map chronographToSample caliber.chronographs
    in
    List.concat <| List.map ungroup calibers


{-| Expects samples to be sorted, as they come from SampleDict.
-}
groupSamples : List Sample -> List Caliber
groupSamples samples =
    let
        loop : List Sample -> Sample -> List Chronograph -> List Caliber -> List Caliber
        loop samps samp chronographs res =
            let
                makeCaliber chrons =
                    if chrons == [] then
                        Nothing

                    else
                        { name = samp.name
                        , weapon = samp.weapon
                        , sort = samp.sort
                        , unit = samp.unit
                        , grains = samp.measurements.grains
                        , diameterInInches = samp.measurements.diameterInInches
                        , chronographs = List.reverse chrons
                        }
                            |> Just
            in
            case samps of
                [] ->
                    case makeCaliber chronographs of
                        Nothing ->
                            List.reverse res

                        Just caliber ->
                            caliber :: List.reverse res

                s :: tail ->
                    let
                        chron =
                            { distance = s.distance
                            , feetPerSecond = s.measurements.feetPerSecond
                            }
                    in
                    if s.name == samp.name && s.weapon == samp.weapon then
                        loop tail samp (chron :: chronographs) res

                    else
                        let
                            newres =
                                case makeCaliber chronographs of
                                    Nothing ->
                                        res

                                    Just caliber ->
                                        caliber :: res
                        in
                        loop tail s [ chron ] newres
    in
    loop samples emptySample [] []


putSamples : SampleDict -> Cmd Msg
putSamples samples =
    let
        calibers =
            if samples == initialSampleDict then
                Nothing

            else
                dictToSamples samples
                    |> groupSamples
                    |> JE.list encodeCaliber
                    |> Just
    in
    put pk.calibers calibers


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , calibers = "calibers"
    }


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get <| Debug.log "get" key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (LocalStorage.getLabeled label <|
            Debug.log ("getLabeled " ++ label) key
        )


localStoragePrefix : String
localStoragePrefix =
    "muzzle-energy"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)



---
--- Rendering
---


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


numberInputWithWidth : String -> (String -> Msg) -> String -> Html Msg
numberInputWithWidth width wrapper v =
    input
        [ style "width" width
        , onInput wrapper
        , value v
        ]
        []


numberInput : (String -> Msg) -> String -> Html Msg
numberInput =
    numberInputWithWidth "5em"


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


oneDigit =
    Locales.Exact 1


threeDigits =
    Locales.Exact 3


renderDialog : Model -> Html Msg
renderDialog model =
    case model.dialog of
        NoDialog ->
            text ""

        EditDialog ->
            editDialog model


view : Model -> Document Msg
view model =
    { title = "Muzzle Energy"
    , body =
        [ renderDialog model
        , div
            [ style "width" "40em"
            , style "max-width" "95%"
            , style "padding" "1em"
            ]
          <|
            renderPage model
        ]
    }


renderPage : Model -> List (Html Msg)
renderPage model =
    let
        inputs =
            model.inputs

        energy =
            model.energy
    in
    [ h2 [] [ text "Muzzle Energy" ]
    , renderSampleDisplay True model.sample "unknown caliber and distance."
    , table [] <|
        List.concat
            [ inputRows True liveSetters inputs
            , [ tr [ td [ text special.nbsp ] ]
              , tr
                    [ td [ b "Energy (foot pounds): " ]
                    , td [ numberDisplay zeroDigits energy.footPounds ]
                    ]
              , tr
                    [ td [ b "Efficacy (energy x area): " ]
                    , td [ numberDisplay zeroDigits energy.efficacy ]
                    ]
              ]
            ]
    , p []
        [ text "The top three rows above are active. Change any number and everything dependent on it will be recomputed. Click a button below to fill in values for the linked load. Click the \"Edit\" button below to change the list of loads."
        ]
    , renderSamples False Nothing SetSample model.samples model
    , p []
        [ button (SetDialog EditDialog) "Edit" ]
    , p []
        [ b "Efficacy"
        , text " is a measure proposed by "
        , a [ href "http://lneilsmith.org/" ]
            [ text "L. Neil Smith" ]
        , text ". It is defined as energy in foot pounds multiplied by projectile cross-sectional area in square inches. Neil says that this is a pretty good indicator of the relative efficacy against live targets of different projectiles and loads. In an email about this page, Neil wrote, \"I'm not absolutely certain of its applicability to rifles (although it looks pretty good and is fine for slugs and rifles like .45/70). There are other factors at work above 2000-2500 feet per second. But every year that passes convinces me more that this is the perfect program for predicting handgun performance.\""
        ]
    , p []
        [ text "Copyright "
        , text special.copyright
        , text "2019-2025, Bill St. Clair"
        , br
        , a [ href "https://elm-lang.org" ]
            [ text "Elm" ]
        , text " "
        , a [ href "https://github.com/billstclair/muzzle-energy/" ]
            [ text "GitHub" ]
        , text " "
        , a [ href "old/" ]
            [ text "Old" ]
        ]
    ]


renderSampleDisplay : Bool -> Maybe SampleDisplay -> String -> Html Msg
renderSampleDisplay showDistance sample unknown =
    p
        [ style "padding-bottom" "0"
        , style "margin-bottom" "0"
        ]
        [ case sample of
            Nothing ->
                b unknown

            Just { name, unit, distance } ->
                Html.b []
                    [ text name
                    , if not showDistance then
                        text ""

                      else
                        span []
                            [ text " at "
                            , if distance == 0 then
                                text "muzzle"

                              else
                                span []
                                    [ text <| String.fromInt distance
                                    , text " "
                                    , text <| unitToString unit
                                    ]
                            ]
                    , text ":"
                    ]
        ]


type alias Setters =
    { grains : String -> Msg
    , ounces : String -> Msg
    , feetPerSecond : String -> Msg
    , inches : String -> Msg
    , gauge : String -> Msg
    }


liveSetters : Setters
liveSetters =
    { grains = SetGrains
    , ounces = SetOunces
    , feetPerSecond = SetFeetPerSecond
    , inches = SetInches
    , gauge = SetGauge
    }


editSetters : Setters
editSetters =
    { grains = SetEditGrains
    , ounces = SetEditOunces
    , feetPerSecond = SetEditFeetPerSecond
    , inches = SetEditInches
    , gauge = SetEditGauge
    }


inputRows : Bool -> Setters -> Inputs -> List (Html Msg)
inputRows showVelocity setters inputs =
    [ tr
        [ td [ b "Bullet Weight (grains): " ]
        , td
            [ numberInput setters.grains inputs.grains ]
        , td [ b "(ounces): " ]
        , td
            [ numberInput setters.ounces inputs.ounces ]
        ]
    , if not showVelocity then
        text ""

      else
        tr
            [ td [ b "Velocity (feet/second): " ]
            , td
                [ numberInput setters.feetPerSecond inputs.fps ]
            ]
    , tr
        [ td [ b "Bullet diameter (inches): " ]
        , td
            [ numberInput setters.inches inputs.inches ]
        , td [ b "(gauge): " ]
        , td
            [ numberInput setters.gauge inputs.gauge ]
        ]
    ]


br : Html Msg
br =
    Html.br [] []


renderSamples : Bool -> Maybe SampleDisplay -> (Sample -> Msg) -> SampleDict -> Model -> Html Msg
renderSamples showSort selectedSample wrapper sampleDict model =
    let
        selected =
            case selectedSample of
                Nothing ->
                    Nothing

                Just sel ->
                    Just { sel | distance = 0 }

        renderWeapons : List Sample -> List (Html Msg)
        renderWeapons samples =
            case samples of
                [] ->
                    []

                { weapon, name, distance } :: _ ->
                    let
                        ( names, tail ) =
                            snarfWeapon weapon samples []
                    in
                    List.concat
                        [ [ p [] <|
                                List.append
                                    [ b <| weaponToString weapon
                                    , text ":"
                                    , br
                                    ]
                                    (renderNames names)
                          ]
                        , renderWeapons tail
                        ]

        snarfWeapon : Weapon -> List Sample -> List Sample -> ( List Sample, List Sample )
        snarfWeapon weapon samples res =
            case samples of
                [] ->
                    ( List.reverse res, [] )

                sample :: tail ->
                    if sample.weapon /= weapon then
                        ( List.reverse res, samples )

                    else
                        snarfWeapon weapon tail <| sample :: res

        -- Render the calibers for a single weapon
        renderNames : List Sample -> List (Html Msg)
        renderNames samples =
            case samples of
                [] ->
                    []

                { name } :: _ ->
                    let
                        ( nameSamples, tail ) =
                            snarfName name samples []
                    in
                    List.concat
                        [ renderName nameSamples 0 []
                        , [ br ]
                        , renderNames tail
                        ]

        renderName : List Sample -> Int -> List (Html Msg) -> List (Html Msg)
        renderName nameSamples index res =
            case nameSamples of
                [] ->
                    List.intersperse (text " ") <| List.reverse res

                sample :: tail ->
                    let
                        { name, sort, unit, distance, measurements } =
                            sample

                        sampleDisplay =
                            sampleToDisplay sample

                        x =
                            if not showSort then
                                text ""

                            else
                                Html.button
                                    [ onClick <| DeleteEditSample sample
                                    , title "Delete this load."
                                    ]
                                    [ text "x" ]

                        isSelected =
                            Just { sampleDisplay | distance = 0 } == selected
                    in
                    if isSelected then
                        let
                            fps =
                                case Dict.get sample.distance model.caliberInputs of
                                    Nothing ->
                                        ""

                                    Just s ->
                                        s

                            inp =
                                numberInput (SetEditVelocity sample) fps

                            elt =
                                if index == 0 then
                                    span []
                                        [ x
                                        , text " "
                                        , text name
                                        , if showSort then
                                            span []
                                                [ text " "
                                                , Html.button
                                                    [ onClick <|
                                                        MoveEditSort sample SortDown
                                                    , title "Move this load down in the list for its weapon."
                                                    ]
                                                    [ text "v" ]
                                                , Html.button
                                                    [ onClick <|
                                                        MoveEditSort sample SortUp
                                                    , title "Move this load up in the list for its weapon."
                                                    ]
                                                    [ text "^" ]
                                                , text " "
                                                , b <| unitToString sample.unit
                                                , text ": "
                                                , numberInput
                                                    (SetEditNewDistance sample)
                                                    model.editNewInputs.newDistance
                                                , text " "
                                                , Html.button
                                                    [ onClick <|
                                                        AddEditDistance sample
                                                    , title "Add the distance to the list for this load."
                                                    , disabled <|
                                                        Nothing
                                                            == String.toInt
                                                                model.editNewInputs.newDistance
                                                    ]
                                                    [ text "+" ]
                                                ]

                                          else
                                            text ""
                                        , br
                                        , text <|
                                            String.repeat 4 special.nbsp
                                                ++ "muzzle: "
                                        , inp
                                        , text " "
                                        ]

                                else if index == 1 then
                                    span []
                                        [ text <|
                                            String.fromInt distance
                                                ++ " "
                                                ++ unitToString unit
                                                ++ ": "
                                        , inp
                                        , text " "
                                        ]

                                else
                                    span []
                                        [ text <|
                                            String.fromInt distance
                                                ++ ": "
                                        , inp
                                        , text " "
                                        ]

                            html =
                                if index < 1 then
                                    elt

                                else
                                    let
                                        tit =
                                            "Delete "
                                                ++ String.fromInt sample.distance
                                                ++ " "
                                                ++ unitToString sample.unit
                                                ++ " distance."
                                    in
                                    span []
                                        [ elt
                                        , Html.button
                                            [ onClick (DeleteEditDistance sample)
                                            , title tit
                                            ]
                                            [ text "x" ]
                                        , text " "
                                        ]
                        in
                        renderName tail (index + 1) <| html :: res

                    else
                        let
                            elt =
                                if index == 0 then
                                    text name

                                else if index == 1 then
                                    text <|
                                        String.fromInt distance
                                            ++ " "
                                            ++ unitToString unit

                                else
                                    text <| String.fromInt distance

                            link =
                                Html.button [ onClick <| wrapper sample ]
                                    [ elt ]

                            html =
                                if index /= 0 then
                                    link

                                else
                                    span []
                                        [ x
                                        , text " "
                                        , link
                                        ]
                        in
                        renderName tail (index + 1) <| html :: res

        snarfName name samples res =
            case samples of
                [] ->
                    ( List.reverse res, [] )

                sample :: tail ->
                    if sample.name /= name then
                        ( List.reverse res, samples )

                    else
                        snarfName name tail <| sample :: res
    in
    div [] <|
        renderWeapons (dictToSamples sampleDict)


dictToSamples : SampleDict -> List Sample
dictToSamples dict =
    Dict.toList dict
        |> List.map Tuple.second


samplesToDict : List Sample -> SampleDict
samplesToDict samples =
    List.map (\s -> ( sampleToKey s, s )) samples
        |> Dict.fromList



---
--- Buttons
---


titledButton : String -> Bool -> Msg -> String -> Html Msg
titledButton theTitle enabled msg label =
    Html.button
        [ onClick msg
        , disabled <| not enabled
        , title theTitle
        ]
        [ b label ]


enabledButton : Bool -> Msg -> String -> Html Msg
enabledButton =
    titledButton ""


button : Msg -> String -> Html Msg
button =
    enabledButton True



---
--- Dialogs
---


editDialog : Model -> Html Msg
editDialog model =
    Dialog.render
        { styles =
            [ ( "max-width", "95%" )
            , ( "max-height", "90%" )
            , ( "overflow", "auto" )
            ]
        , title = "Edit"
        , content = editDialogContent model
        , actionBar =
            [ button CommitEditDialog "Save"
            , button DismissDialog "Cancel"
            ]
        }
        True


type alias NewInputs =
    { show : Bool
    , name : String
    , weapon : Weapon
    , unit : Unit
    , grains : String
    , ounces : String
    , inches : String
    , gauge : String
    , muzzleFps : String
    , newDistance : String
    }


initialNewInputs : NewInputs
initialNewInputs =
    { show = False
    , name = ""
    , weapon = Rifle
    , unit = Yards
    , grains = "150"
    , ounces = "0.343"
    , inches = "0.308"
    , gauge = "159.087"
    , muzzleFps = "2820"
    , newDistance = ""
    }


editDialogContent : Model -> List (Html Msg)
editDialogContent model =
    let
        editNewInputs =
            model.editNewInputs
    in
    [ renderSampleDisplay False model.editSample "No caliber selected."
    , table [] <|
        inputRows False editSetters model.editInputs
    , if editNewInputs.show then
        Html.button [ onClick <| SetEditNewShow False ]
            [ text "Hide New Load UI" ]

      else
        Html.button [ onClick <| SetEditNewShow True ]
            [ text "Show New Load UI" ]
    , if not editNewInputs.show then
        text ""

      else
        table []
            [ tr
                [ Html.td [ colspan 4 ]
                    [ b "Name: "
                    , numberInputWithWidth "20em" SetEditNewName editNewInputs.name
                    ]
                ]
            , tr
                [ td [ b "Weapon: " ]
                , td [ editWeaponSelector editNewInputs.weapon ]
                , td [ b " Unit: " ]
                , td [ editUnitSelector editNewInputs.unit ]
                ]
            , tr
                [ td [ b "Grains: " ]
                , td [ numberInput SetEditNewGrains editNewInputs.grains ]
                , td [ b "ounces: " ]
                , td [ numberInput SetEditNewOunces editNewInputs.ounces ]
                ]
            , tr
                [ td [ b "Inches: " ]
                , td [ numberInput SetEditNewInches editNewInputs.inches ]
                , td [ b "gauge: " ]
                , td [ numberInput SetEditNewGauge editNewInputs.gauge ]
                ]
            , tr
                [ td [ b "Muzzle FPS: " ]
                , td [ numberInput SetEditNewMuzzleFps editNewInputs.muzzleFps ]
                , td
                    [ Html.button
                        [ onClick NewEditCaliber
                        , disabled (not <| newEditCaliberOk model)
                        , title "Use Name, Weapon, Unit, Grains, Inches, Muzzle FPS to create a new load."
                        ]
                        [ text "New Load" ]
                    ]
                ]
            ]
    , renderSamples True model.editSample SetEditSample model.editSamples model
    , Html.button [ onClick RestoreDefaultSamples ]
        [ text "Restore Defaults" ]
    ]


newEditCaliberOk : Model -> Bool
newEditCaliberOk model =
    let
        editNewInputs =
            model.editNewInputs

        name =
            editNewInputs.name

        weapon =
            editNewInputs.weapon

        unit =
            editNewInputs.unit

        grains =
            editNewInputs.grains

        inches =
            editNewInputs.inches

        muzzleFps =
            editNewInputs.muzzleFps

        display =
            SampleDisplay name weapon 0 unit 0

        editSamples =
            dictToSamples model.editSamples

        pred sample =
            let
                disp =
                    sampleToDisplay sample
            in
            display == { disp | sort = 0, distance = 0 }
    in
    ("" /= name)
        && (Nothing /= String.toFloat grains)
        && (Nothing /= String.toFloat inches)
        && (Nothing /= String.toFloat muzzleFps)
        && (Nothing == LE.find pred editSamples)


editWeaponSelector : Weapon -> Html Msg
editWeaponSelector weapon =
    select [ onInput SetEditNewWeapon ]
        [ option
            [ value "Rifle"
            , selected <| Rifle == weapon
            ]
            [ text "Rifle" ]
        , option
            [ value "Handgun"
            , selected <| Handgun == weapon
            ]
            [ text "Handgun" ]
        , option
            [ value "Shotgun"
            , selected <| Shotgun == weapon
            ]
            [ text "Shotgun" ]
        ]


editUnitSelector : Unit -> Html Msg
editUnitSelector unit =
    select [ onInput SetEditNewUnit ]
        [ option
            [ value "Feet"
            , selected <| Feet == unit
            ]
            [ text "Feet" ]
        , option
            [ value "Yards"
            , selected <| Yards == unit
            ]
            [ text "Yards" ]
        ]



---
--- Special characters
---


stringFromCode : Int -> String
stringFromCode code =
    String.fromList [ Char.fromCode code ]


special =
    { nbsp = stringFromCode 160 -- \u00A0
    , copyright = stringFromCode 169 -- \u00A9
    , biohazard = stringFromCode 9763 -- \u2623
    , black_star = stringFromCode 10036 -- \u2734
    , hourglass = stringFromCode 8987 -- \u231B
    , hourglass_flowing = stringFromCode 9203 -- \u23F3
    }
