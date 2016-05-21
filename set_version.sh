#!/bin/bash

vsn=$1

sed -i '' "s/{vsn, .*/{vsn, \"$vsn\"},/" apps/glot/src/glot.app.src
sed -i '' "s/{default_release, glot.*/{default_release, glot, \"$vsn\"}./" config/relx.config
sed -i '' "s/{release, {glot.*/{release, {glot, \"$vsn\"}, [glot]}./" config/relx.config
