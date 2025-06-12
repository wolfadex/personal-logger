module Css exposing (logList, previousLogTitle, previousLogContent, settingsButton, loader, theme_1, theme_2, theme_3, theme_4, theme_5, theme_6)

import Html
import Html.Attributes


logList : Html.Attribute msg
logList =
    Html.Attributes.class "logList"


previousLogTitle : Html.Attribute msg
previousLogTitle =
    Html.Attributes.class "previousLogTitle"


previousLogContent : Html.Attribute msg
previousLogContent =
    Html.Attributes.class "previousLogContent"


settingsButton : Html.Attribute msg
settingsButton =
    Html.Attributes.class "settingsButton"


loader : Html.Attribute msg
loader =
    Html.Attributes.class "loader"


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
