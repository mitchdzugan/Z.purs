[parallel]
slp-db-web: slp-db-web-bundle-watch slp-db-web-serve

slp-db-web-bundle-watch:
    vite build test/Web/SlpDB/ -w

slp-db-web-serve:
    serve -s ./test/Web/SlpDB/dist/

slp-db *args:
    spago run -m Test.SlpDB -- {{args}}

slp-id *args:
    spago run -m Test.SlpID -- {{args}}

slp-rec *args:
    spago run -m Test.SlpRec -- {{args}}
