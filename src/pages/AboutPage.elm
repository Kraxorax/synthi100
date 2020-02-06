module AboutPage exposing (page)

import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import Model exposing (Model)
import Msg exposing (Msg)
import Styles exposing (theGray)

mainColumnCss : Style
mainColumnCss =
    Css.batch
        [ boxSizing borderBox
        , borderWidth2 (px 0) (px 10)
        , borderStyle solid
        , borderColor (hex "#000000")
        ]

headingCss =
    batch
        [ lineHeight (num 1.45)
        , borderTop2 (px 1) solid
        , letterSpacing (px 0.9)
        , borderBottom2 (px 1) solid
        , marginBottom (px 14)
        , marginTop (px 0)
        , fontSize (px 36)
        , fontWeight bold
        , boxSizing borderBox
        ]

creditsCss : Style
creditsCss =
    batch
        [ lineHeight (px 22.5)
        , letterSpacing (px 0.16)
        , color (hex "ffffff")
        , position relative
        , height (pct 100)
        , padding (px 20)
        ]

page : Model -> Html Msg
page model =
    div
        [ css
            [ padding2 (px 0) (px 10)
            , displayFlex
            , color (hex "000")
            , fontSize (px 14)
            , boxSizing borderBox
            ]
        ]
        [ div
            [ css
                [ mainColumnCss
                , flex (pct (100*1/3.0))
                , backgroundColor theGray
                ]
            ]
            [ credits ]
        , div
            [ css
                [ mainColumnCss
                , flex (pct (100*2/3.0))
                , backgroundColor (hex "c8c8c8")
                ]
            ]
            [ about ]

        ]

credits =
    section
        [ id "credits"
        , css [ creditsCss ]
        ]
        [ h1 [ css [ headingCss ] ] [ text "CREDITS"  ]
        , h2 [ css [ fontSize (px 14), marginBottom (px 20) ] ] [text "SYNTHI ON THE WEB TEAM" ]  
        , p [] [ text "Project leader: ", strong [] [ text "Ksenija Stevanović" ] ]
        , p []
            [ text "User interface design / "
            , br [] []
            , text "Project co\u{2011}ordinator: ", strong [] [ text "Svetlana Maraš" ]
            ]
        , p [] [ text "Software development:", br [] [],  strong [] [text "Bojan Petrović / Marko Kralj" ] ]
        , p [] [ text "Graphic design: ", strong [] [ text "Andrej Dolinka" ] ]
        ----------
        , hr [ css [ marginTop (px 18), marginBottom (px 22), borderColor (hex "ffffff") ] ] []
        ----------
        , h2 [ css [ fontSize (px 14), marginBottom (px 23)  ] ]
                  [ text "CONTRIBUTING ARTISTS"
                  , br [] []
                  , text "FOR THE SOUND DATABASE"
                  ]
        , p [] [ strong [] [ text "Paul Pignon" ] ]
        , p [] [ strong [] [ text "Paul Oomen" ] ]
        , p [] [ strong [] [ text "Derek Holzer" ] ]
        , p [] [ strong [] [ text "Nicola Ratti" ] ]
        , p [] [ strong [] [ text "Robert Lippok" ] ]
        , p [] [ strong [] [ text "Rastko Lazić" ] ]
        , hr [ css [ marginTop (px 11), borderColor (hex "ffffff") ] ] []
        ]

about : Html msg
about =
    section
        [ css
            [ padding (px 20) ] ]
        [ h1
            [ css
                [ headingCss , width (pct 45)]]
                [ span [] [ text "ABOUT" ]
            ]
        , theText
        ]

theText : Html msg
theText =
    div
       [ id "about"
       , css
            [ property "column-count" "2"
            , property "column-gap" "5%"
            , lineHeight (px 18.5)
            , letterSpacing (px 0.16)
            ]
       ]
       (List.map (\line -> p [] [text line]) aboutLines)

aboutLines : List String
aboutLines =
   String.split "%" """

The SYNTHI ON THE WEB project is dedicated to the digital extension of
the legendary EMS SYNTHI 100 which belongs to the Electronic Music
Studio of Radio Belgrade, part of the public broadcasting organisation
Radio-Television Serbia. In 2017, Radio Belgrade 3, in its capacity as
the founding program of the Electronic Music Studio, made considerable
institutional efforts to restore this analogue instrument, thus
opening new possibilities for its use in the digital era.

%

Working with experts — electronic musicians, sound engineers, software
developers and designers — an online library/database of SYNTHI 100
fabled sounds was created, free to use, download and
share. Additionally, a twofold notational system for SYNTHI 100
patches was developed enabling both textual notation and reactive
graphical representation. In the first case, inputs, outputs and
values are simply converted into numbers and abbreviations which allow
for easy download and translation into a patch once you are working
with the real EMS SYNTHI 100.

%

The second system illustrates more closely the actual patching
process, and allows exploration of the SYNTHI’s famous interface
through the digital realm of reactive computer graphics which mimics
to a certain degree the actual physical instrument.

%

In this way, the vast possibilities of EMS SYNTHI 100 are made more
transparent for the general public whose access to this rare
instrument is limited.

%

This website is part of the ongoing research of the Electronic Music
Studio of Radio Belgrade and Radio Belgrade 3 into the creative
possibilities of EMS SYNTHI 100, sharing its rich heritage with
professionals and enthusiasts worldwide, while emphasizing the
educational and creative potential of such projects within the sphere
of modern radio and beyond.

%

This is an ongoing project initiated in 2017 and supported by the EBU
Innovation Fund.

"""