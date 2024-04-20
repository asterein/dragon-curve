#! /bin/bash

source .make.env
elm make src/$MAIN --output=$OUTFILE
