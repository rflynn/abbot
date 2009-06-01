#!/bin/sh

erl \
  -pz $(pwd) \
  -pz $(pwd)/plugin \
  -pz $(pwd)/htmlparser \
  -s inets start

