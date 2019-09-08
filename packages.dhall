
let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.2-20190725/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190831/packages.dhall sha256:852cd4b9e463258baf4e253e8524bcfe019124769472ca50b316fe93217c3a47

let overrides =
      { node-buffer =
              upstream.node-buffer
          //  { dependencies =
                  [ "effect"
                  , "maybe"
                  , "arraybuffer-types"
                  , "st"
                  , "unsafe-coerce"
                  ]
              , repo =
                  "https://github.com/purescript-node/purescript-node-buffer.git"
              , version =
                  "v6.0.0"
              }
      }

let additions = {=}

in  upstream // overrides // additions
