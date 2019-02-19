module TestPage exposing (testPage)

import Html
import Knob exposing (KnobMsg, simpleKnobSvg, simpleSwitchSvg)
import Model exposing (Control(..), Model, PinConnection)
import Msg exposing (..)
import Patch as P
import PinTable


testPage : Model -> List (Html.Html Msg)
testPage model =
    let
        ss =
            case model.synthiSchema of
                Just s ->
                    s

                Nothing ->
                    { attributes = [], audioPanel = [], controlPanel = [], modules = [] }

        audioPins =
            case model.patches of
                Just patches ->
                    (patches |> List.head |> Maybe.withDefault P.noPatch).audioPins

                Nothing ->
                    []

        activeAudioPinText =
            activePinToString model.activeAudioPin

        hoverAudioPinText =
            activePinToString model.hoverAudioPin

        ( om, im ) =
            case model.activeModules of
                Just mdls ->
                    mdls

                nothing ->
                    ( { name = "no module", controls = [] }, { name = "no module", controls = [] } )

        activeModulesText =
            om.name ++ " >> " ++ im.name
    in
    [ Html.div []
        [ Html.div []
            [ Html.div [] [ Html.text activeAudioPinText ]
            , Html.div [] [ Html.text hoverAudioPinText ]
            , Html.div [] [ Html.text activeModulesText ]
            , Html.map reactToAudioPinEvent (PinTable.audioPanel ss audioPins model.pinModel)
            , Html.div []
                [ Html.div [] [ Html.text "out module:" ]
                , Html.map (\kmsg -> KnobEvent kmsg) (controlsToSvg om.controls)
                ]
            , Html.div []
                [ Html.div [] [ Html.text "in module:" ]
                , Html.map (\kmsg -> KnobEvent kmsg) (controlsToSvg im.controls)
                ]
            ]
        ]
    ]


activePinToString : Maybe PinConnection -> String
activePinToString pinMods =
    case pinMods of
        Just pm ->
            pm.out ++ " --> " ++ pm.into

        Nothing ->
            ""


reactToAudioPinEvent : PinTable.PinMsg -> Msg
reactToAudioPinEvent pinMsg =
    case pinMsg of
        PinTable.PinClick ( x, y ) ->
            AudioPinClick ( x, y )

        PinTable.PinIn ( x, y ) ->
            AudioPinHover ( x, y )

        _ ->
            PinEvent pinMsg


controlsToSvg : List Control -> Html.Html KnobMsg
controlsToSvg cs =
    Html.div []
        (cs
            |> List.map
                (\ctrl ->
                    case ctrl of
                        KnobCtrl { name, value } ->
                            simpleKnobSvg value

                        SwitchCtrl { name, case_ } ->
                            simpleSwitchSvg case_
                )
        )
