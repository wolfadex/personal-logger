module Css exposing (inputError, logList, previousLog, previousLogTitle, previousLogContent, storeChangesButton, settingsButton, settingsModel, kofiButton, modalHeader, loader, loaderSimple, theme_1, theme_2, theme_3, theme_4, theme_5, theme_6)

import Html
import Html.Attributes


inputError : Html.Attribute msg
inputError =
    Html.Attributes.class "inputError"


logList : Html.Attribute msg
logList =
    Html.Attributes.class "logList"


previousLog : Html.Attribute msg
previousLog =
    Html.Attributes.class "previousLog"


previousLogTitle : Html.Attribute msg
previousLogTitle =
    Html.Attributes.class "previousLogTitle"


previousLogContent : Html.Attribute msg
previousLogContent =
    Html.Attributes.class "previousLogContent"


storeChangesButton : Html.Attribute msg
storeChangesButton =
    Html.Attributes.class "storeChangesButton"


settingsButton : Html.Attribute msg
settingsButton =
    Html.Attributes.class "settingsButton"


settingsModel : Html.Attribute msg
settingsModel =
    Html.Attributes.class "settingsModel"


kofiButton : Html.Attribute msg
kofiButton =
    Html.Attributes.class "kofiButton"


modalHeader : Html.Attribute msg
modalHeader =
    Html.Attributes.class "modalHeader"


loader : Html.Attribute msg
loader =
    Html.Attributes.class "loader"


loaderSimple : Html.Attribute msg
loaderSimple =
    Html.Attributes.class "loaderSimple"


theme_1 : Html.Attribute msg
theme_1 =
    Html.Attributes.class "theme_1"


theme_2 : Html.Attribute msg
theme_2 =
    Html.Attributes.class "theme_2"


theme_3 : Html.Attribute msg
theme_3 =
    Html.Attributes.class "theme_3"


theme_4 : Html.Attribute msg
theme_4 =
    Html.Attributes.class "theme_4"


theme_5 : Html.Attribute msg
theme_5 =
    Html.Attributes.class "theme_5"


theme_6 : Html.Attribute msg
theme_6 =
    Html.Attributes.class "theme_6"
