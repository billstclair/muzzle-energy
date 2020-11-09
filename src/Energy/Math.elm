-----------------------------------------------------------------
--
-- Math.elm
-- All the math for the muzzle energy computer.
-- Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Energy.Math exposing
    ( Energy
    , ImperialMeasurements
    , Measurements
    , computeEnergy
    , constants
    , diameterInGaugeToInches
    , diameterInInchesToGauge
    , emptyEnergy
    , emptyImperialMeasurements
    , emptyMeasurements
    , grainsToOunces
    , imperialToMeasurements
    , measurementsToImperial
    , ouncesToGrains
    )


type alias Measurements =
    { grains : Float
    , feetPerSecond : Float
    , diameterInInches : Float
    , ounces : Float
    , gauge : Float
    }


emptyMeasurements : Measurements
emptyMeasurements =
    Measurements 0 0 0 0 0


type alias ImperialMeasurements =
    { grams : Float
    , metersPerSecond : Float
    , diameterInMm : Float
    }


emptyImperialMeasurements : ImperialMeasurements
emptyImperialMeasurements =
    ImperialMeasurements 0 0 0


type alias Energy =
    { footPounds : Float
    , efficacy : Float
    , sectionalDensity : Float
    }


emptyEnergy : Energy
emptyEnergy =
    Energy 0 0 0


constants =
    { grainsPerLb = 7000.0
    , lbsPerSlug = 32.17405
    , grainsPerSlug = 7000.0 * 32.17405
    , gaugeTimesInchesCubed = 4.648224
    , feetPerMeter = 3.281
    , grainsPerGram = 15.429718
    , cmPerInch = 2.54
    }


imperialToMeasurements : ImperialMeasurements -> Measurements
imperialToMeasurements im =
    { grains = constants.grainsPerGram * im.grams
    , feetPerSecond = constants.feetPerMeter * im.metersPerSecond
    , diameterInInches = im.diameterInMm / (10 * constants.cmPerInch)
    , ounces = 0
    , gauge = 0
    }
        |> grainsToOunces
        |> diameterInInchesToGauge


measurementsToImperial : Measurements -> ImperialMeasurements
measurementsToImperial m =
    { grams = m.grains / constants.grainsPerGram
    , metersPerSecond = m.feetPerSecond / constants.feetPerMeter
    , diameterInMm = m.diameterInInches * constants.cmPerInch * 10
    }


grainsToOunces : Measurements -> Measurements
grainsToOunces m =
    { m | ounces = 16 * m.grains / 6999 }


ouncesToGrains : Measurements -> Measurements
ouncesToGrains m =
    { m | grains = m.ounces * 6999 / 16 }


diameterInInchesToGauge : Measurements -> Measurements
diameterInInchesToGauge m =
    { m | gauge = constants.gaugeTimesInchesCubed / (m.diameterInInches ^ 3) }


diameterInGaugeToInches : Measurements -> Measurements
diameterInGaugeToInches m =
    { m | diameterInInches = (constants.gaugeTimesInchesCubed / m.gauge) ^ (1 / 3) }


computeEnergy : Measurements -> Energy
computeEnergy m =
    let
        footPounds =
            0.5 * (m.grains / constants.grainsPerSlug) * (m.feetPerSecond ^ 2)

        r =
            m.diameterInInches / 2

        efficacy =
            footPounds * pi * r * r

        secdens =
            (m.grains / constants.grainsPerLb)
                / (m.diameterInInches ^ 2)
    in
    { footPounds = footPounds
    , efficacy = efficacy
    , sectionalDensity = secdens
    }
