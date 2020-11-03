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


module Energy.Math exposing (Energy, Measurements, computeEnergy)


type alias Measurements =
    { grains : Float
    , feetPerSecond : Float
    , diameterInInches : Float
    , ounces : Float
    , gauge : Float
    }


type alias ImperialMeasureMents =
    { grams : Float
    , metersPerSecond : Float
    , diameterInMm : Float
    }


type alias Energy =
    { footPounds : Float
    , efficacy : Float
    , sectionalDensity : Float
    }


constants =
    { grainsPerLb = 7000.0
    , lbsPerSlug = 32.17405
    , grainsPerSlug = 7000.0 * 32.17405
    , gaugeTimesInchesCubed = 4.648224
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
