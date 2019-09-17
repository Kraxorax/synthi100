module AboutPage exposing (page)

import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Model exposing (Model)
import Msg exposing (Msg)


page : Model -> Html Msg
page model =
    div [ css [ Css.displayFlex, flex auto, width (pct 100), color (hex "000"), fontSize (px 14) ] ]
        [ div [ css [ width (pct 26), marginRight (px 20), backgroundColor (hex "9b9b9b"), padding4 (px 30) (px 58) (px 5) (px 30) ] ]
            [ div [ css [ headerCss, width (pct 100) ] ]
                [ span [] [ text "CREDITS" ] ]
            , credits
            ]
        , div [ css [ flex (int 2), backgroundColor (hex "c8c8c8"), padding4 (px 30) (px 58) (px 5) (px 30) ] ]
            [ div [ css [ headerCss, width (pct 40) ] ]
                [ span [] [ text "ABOUT" ] ]
            , theText
            ]
        ]


headerCss =
    batch
        [ height (px 63)
        , borderTop2 (px 1) solid
        , borderBottom2 (px 1) solid
        , marginBottom (px 30)
        , fontSize (px 36)
        , fontWeight bold
        , padding2 (px 14) (px 0)
        , boxSizing borderBox
        ]


credits =
    div [ css [ color (hex "ffffff") ] ]
        [ p [ css [ fontWeight bold ] ] [ text "SYNTHI ON THE WEB TEAM:" ]
        , br [] []
        , p [] [ text "Project leader: ", b [] [ text "Ksenija Stevanović" ] ]
        , p [] [ text "User interface design /" ]
        , p [] [ text "Project co-ordinator: ", b [] [ text "Svetlana Maraš" ] ]
        , p [] [ text "Software development:" ]
        , p [ css [ fontWeight bold ] ] [ text "Bojan Petrović / Marko Kralj" ]
        , p [] [ text "Graphic design: ", b [] [ text "Andrej Dolinka" ] ]
        , br [] []
        , hr [ css [ borderColor (hex "ffffff") ] ] []
        , br [] []
        , div [ css [ fontWeight bold ] ]
            [ p [] [ text "CONTRIBUTING ARTISTS FOR THE SOUND DATABASE:" ]
            , br [] []
            , p [] [ text "Paul Pignon" ]
            , p [] [ text "Paul Oomen" ]
            , p [] [ text "Derek Holzer" ]
            , p [] [ text "Nicola Ratti" ]
            , p [] [ text "Robert Lippok" ]
            , p [] [ text "Rastko Lazić" ]
            ]
        , br [] []
        , hr [ css [ borderColor (hex "ffffff") ] ] []
        , br [] []
        ]


theText : Html msg
theText =
    div [ css [ displayFlex, lineHeight (num 1.29), letterSpacing (px 0.2), fontWeight (int 500) ] ]
        [ div [ css [ flex (int 1), marginRight (px 40) ] ]
            [ p [] [ text "The SYNTHI ON THE WEB project is dedicated to the digital extension of the legendary EMS SYNTHI 100 which belongs to the Electronic Music Studio of Radio Belgrade, part of the public broadcasting organisation Radio-Television Serbia. In 2017, Radio Belgrade 3, in its capacity as the founding program of the Electronic Music Studio, made considerable institutional efforts to restore this analogue instrument, thus opening new possibilities for its use in the digital era." ]
            , p [] [ text "Working with experts — electronic musicians, sound engineers, software developers and designers — an online library/database of SYNTHI 100 fabled sounds was created, free to use, download and share. Additionally, a twofold notational system for SYNTHI 100 patches was developed enabling both textual notation and reactive graphical representation. In the first case, inputs, outputs and values are simply converted into numbers and abbreviations which allow for easy download and translation into a patch once you are working with the real EMS SYNTHI 100." ]
            ]
        , div [ css [ flex (int 1) ] ]
            [ p [] [ text "The second system illustrates more closely the actual patching process, and allows exploration of the SYNTHI’s famous interface through the digital realm of reactive computer graphics which mimics to a certain degree the actual physical instrument." ]
            , p [] [ text "In this way, the vast possibilities of EMS SYNTHI 100 are made more transparent for the general public whose access to this rare instrument is limited." ]
            , p [] [ text "This website is part of the ongoing research of the Electronic Music Studio of Radio Belgrade and Radio Belgrade 3 into the creative possibilities of EMS SYNTHI 100, sharing its rich heritage with professionals and enthusiasts worldwide, while emphasizing the educational and creative potential of such projects within the sphere of modern radio and beyond." ]
            , p [] [ text "This is an ongoing project initiated in 2017 and supported by the EBU Innovation Fund." ]
            ]
        ]
