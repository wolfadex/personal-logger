module Css exposing (logList, previousLogTitle, previousLogContent)

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
