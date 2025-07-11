module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.ReviewAtDocs
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ Docs.ReviewAtDocs.rule
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories
            [ "src/Evergreen/"
            , "elm-pkg-js/"
            ]
        |> Rule.ignoreErrorsForFiles
            [ "src/Backend.elm"
            , "src/Env.elm"
            , "src/Frontend.elm"
            , "src/Types.elm"
            ]
    , NoImportingEverything.rule [ "Types" ]
    , NoMissingTypeAnnotation.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Env.elm"
            ]
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , NoUnused.CustomTypeConstructors.rule []
        |> Rule.ignoreErrorsForDirectories
            [ "src/Evergreen/"
            ]
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForDirectories
            [ "src/Evergreen/"
            ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories
            [ "src/Evergreen/"
            , "elm-pkg-js/"
            ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    ]
        |> List.map
            (Rule.ignoreErrorsForDirectories [ "tests/" ])
