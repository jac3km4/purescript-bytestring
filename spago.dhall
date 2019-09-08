{ name =
    "bytestring"
, dependencies =
    [ "effect", "catenable-lists", "node-buffer", "functions", "integers" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
