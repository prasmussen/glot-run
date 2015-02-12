#!/bin/bash

# Build plt
# dialyzer --build_plt --apps erts kernel stdlib crypto sasl
# dialyzer --add_to_plt --apps ssl reltool

dialyzer -r apps/*/src --src
