#!/bin/sh
set -eu
cd "$(dirname "$0")"
gosh generate.scm
rsync -crv index.html alpha.servers.scheme.org:/production/registry/www/
