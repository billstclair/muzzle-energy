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


measurementsToInputs : Measurements -> Inputs
measurementsToInputs measurements =
    { grains = digitsFormat zeroDigits measurements.grains
    , ounces = digitsFormat threeDigits measurements.ounces
    , fps = digitsFormat zeroDigits measurements.feetPerSecond
    , inches = digitsFormat threeDigits measurements.diameterInInches
    , gauge = digitsFormat threeDigits measurements.gauge
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
sampleToKey { name, weapon, sort, unit, distance } =
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
    , newEditWeapon : Weapon
    , newEditName : String
    }


type Dialog
    = NoDialog
    | EditDialog


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
    | SetEditVelocity Sample String
    | SetEditSort Sample String
    | SetEditSample Sample
    | SetSample Sample
    | Process Value
    | SetDialog Dialog
    | DismissDialog
    | CommitEditDialog


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
            , newEditWeapon = Rifle
            , newEditName = ""
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
            let
                mdl =
                    case String.toFloat string of
                        Nothing ->
                            model

                        Just grains ->
                            updateSamples False
                                (\sample ->
                                    Debug.log "  sample:" <|
                                        updateSampleMeasurements
                                            (\meas ->
                                                { meas | grains = grains }
                                                    |> Math.grainsToOunces
                                            )
                                            sample
                                )
                                model
            in
            mdl |> withNoCmd

        SetEditOunces string ->
            model |> withNoCmd

        SetEditFeetPerSecond string ->
            model |> withNoCmd

        SetEditInches string ->
            model |> withNoCmd

        SetEditGauge string ->
            model |> withNoCmd

        DeleteEditSample sample ->
            model |> withNoCmd

        SetEditVelocity sample string ->
            model |> withNoCmd

        SetEditSort sample string ->
            model |> withNoCmd

        SetEditSample sample ->
            model |> withNoCmd

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
            let
                inputs2 =
                    case model.editSample of
                        Nothing ->
                            model.inputs

                        Just editSample ->
                            let
                                samp2 =
                                    { editSample | sort = 0 }
                            in
                            case
                                LE.find (sampleMatches True samp2) <|
                                    Dict.toList model.editSamples
                            of
                                Nothing ->
                                    model.inputs

                                Just ( _, sample ) ->
                                    measurementsToInputs sample.measurements
            in
            { model
                | dialog = NoDialog
                , samples = model.editSamples
                , sample = model.editSample
                , inputs = inputs2
            }
                |> withNoCmd


updateSampleMeasurements : (Measurements -> Measurements) -> Sample -> Sample
updateSampleMeasurements updater sample =
    { sample | measurements = updater sample.measurements }


updateSamples : Bool -> (Sample -> Sample) -> Model -> Model
updateSamples matchDistance modfun model =
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
                    }

                newSamples =
                    LE.updateIf (sampleMatches matchDistance dist)
                        (\( key, sample ) -> ( key, modfun sample ))
                    <|
                        Dict.toList model.editSamples

                exactDist =
                    { sampleDisplay | sort = 0 }

                editInputs =
                    case LE.find (sampleMatches True exactDist) newSamples of
                        Nothing ->
                            model.editInputs

                        Just ( _, es ) ->
                            measurementsToInputs es.measurements
            in
            { model
                | editSamples = Dict.fromList newSamples
                , editInputs = editInputs
            }


sampleMatches : Bool -> SampleDisplay -> ( key, Sample ) -> Bool
sampleMatches matchDistance display ( _, sample ) =
    display
        == sampleToDisplay
            { sample
                | sort = 0
                , distance =
                    if matchDistance then
                        sample.distance

                    else
                        -1
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
                get pk.model

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
                                        Debug.log "err" err
                                in
                                mdl2 |> withCmd cmd

                            Ok savedModel ->
                                savedToModel savedModel mdl2 |> withCmd cmd

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


{-| Persistent storage keys
-}
pk =
    { model = "model"
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
                    [ td [ b "Energy: " ]
                    , td [ numberDisplay zeroDigits energy.footPounds ]
                    ]
              , tr
                    [ td [ b "Efficacy (energy x area): " ]
                    , td [ numberDisplay zeroDigits energy.efficacy ]
                    ]
              ]
            ]
    , p []
        [ text "The top three rows above are active. Change any number and everything dependent on it will be recomputed. Click a button below to fill in values for the linked load."
        ]
    , renderSamples False Nothing SetSample model
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


renderSampleDisplay : Bool -> Maybe SampleDisplay -> String -> Html Msg
renderSampleDisplay showDistance sample unknown =
    case sample of
        Nothing ->
            p []
                [ text unknown ]

        Just { name, unit, distance } ->
            p []
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


renderSamples : Bool -> Maybe SampleDisplay -> (Sample -> Msg) -> Model -> Html Msg
renderSamples showSort selectedSample wrapper model =
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
                            Html.button
                                [ onClick <| DeleteEditSample sample
                                , title "Delete this load."
                                ]
                                [ text "x" ]

                        sortStr =
                            digitsFormat oneDigit sort
                    in
                    if Just { sampleDisplay | distance = 0 } == selected then
                        let
                            fps =
                                digitsFormat zeroDigits measurements.feetPerSecond

                            inp =
                                numberInput (SetEditVelocity sample) fps

                            elt =
                                if index == 0 then
                                    span []
                                        [ if showSort then
                                            span []
                                                [ numberInputWithWidth "2em"
                                                    (SetEditSort sample)
                                                    sortStr
                                                , text ": "
                                                , x
                                                , text " "
                                                ]

                                          else
                                            text ""
                                        , text name
                                        , text ", muzzle: "
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
                        in
                        renderName tail (index + 1) <| elt :: res

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
                                span []
                                    [ if index == 0 && showSort then
                                        span []
                                            [ text <| sortStr ++ ": "
                                            , x
                                            , text " "
                                            ]

                                      else
                                        text ""
                                    , Html.button [ onClick <| wrapper sample ]
                                        [ elt ]
                                    ]
                        in
                        renderName tail (index + 1) <| link :: res

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
        renderWeapons (dictToSamples model.samples)


dictToSamples : SampleDict -> List Sample
dictToSamples dict =
    Dict.toList dict
        |> List.map Tuple.second



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


editDialogContent : Model -> List (Html Msg)
editDialogContent model =
    [ renderSampleDisplay False model.editSample "No caliber selected."
    , table [] <|
        inputRows False editSetters model.editInputs
    , renderSamples True model.editSample SetEditSample model
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
