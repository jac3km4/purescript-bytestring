{ name =
    "bytestring"
, dependencies =
    [ "effect", "catenable-lists", "node-buffer", "functions" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
