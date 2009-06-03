#!/bin/sh

# set the appropriate environment in the `erl` shell

erl \
  +K true \
  -pz $(pwd) \
  -pz $(pwd)/plugin \
  -pz $(pwd)/htmlparser \
  -s inets start

