#!/bin/sh
set -eu
cd "$(dirname "$0")"
gosh generate.scm
curl --location --fail --silent --show-error -o schemeorg.css \
    https://www.scheme.org/schemeorg.css
rsync -crv index.html *.css *.pose tuonela.scheme.org:/production/registry/www/
