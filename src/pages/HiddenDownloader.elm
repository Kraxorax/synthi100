module HiddenDownloader exposing (hiddenDownloader)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)

-- Files can be downloaded only if the user
-- is logged in. The login process is initiated
-- when the unauthenticated user clicks on the
-- download link. We remember the file which
-- the user wants to download and then redirect
-- to SSO. When the SSO redirects us back,
-- we land on the page from which the download was
-- initiated, and we start the download by
-- displaying an iframe with the "src" set to
-- the file we remembered above. The location
-- of the file is in the "download" query
-- parameter.
hiddenDownloader : Maybe String -> Html msg
hiddenDownloader file =
    case file of
        Just path -> iframe [src path, css [display none]] []
        Nothing -> text ""
