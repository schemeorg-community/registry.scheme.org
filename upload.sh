#!/bin/sh
set -eu
cd "$(dirname "$0")"
gosh generate.scm
rsync -crv index.html *.pose alpha.servers.scheme.org:/production/registry/www/
