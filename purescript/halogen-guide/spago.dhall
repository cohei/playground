{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
        [ "affjax"
        , "console"
        , "effect"
        , "foldable-traversable"
        , "halogen"
        , "integers"
        , "psci-support"
        , "random"
        ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
