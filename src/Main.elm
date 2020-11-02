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
        , td
        , text
        , textarea
        , th
        , tr
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


type alias Model =
    { cmdPort : Value -> Cmd Msg
    }


type Page
    = HomePage
    | ColumnsPage
    | ExplorerPage


type Msg
    = Noop
    | OnUrlRequest UrlRequest
    | OnUrlChange Url


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
    { cmdPort =
        PortFunnels.getCmdPort (\v -> Noop) "foo" False
    }
        |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ PortFunnels.subscriptions (\v -> Noop) model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal _ ->
                    model |> withNoCmd

                External url ->
                    model |> withCmd (openWindow <| JE.string url)

        OnUrlChange url ->
            model |> withNoCmd


view : Model -> Document Msg
view model =
    { title = "Muzzle Energy"
    , body =
        [ h2 [] [ text "Muzzle Energy" ]
        , p []
            [ text "A new, smarter, muzzle energy computer."
            ]
        , p []
            [ a [ href "https://elm-lang.org" ]
                [ text "Elm" ]
            , text " "
            , a [ href "https://github.com/billstclair/muzzle-energy/" ]
                [ text "GitHub" ]
            ]
        ]
    }
