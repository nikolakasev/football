#!/bin/bash
elm-make Main.elm --output main.js
cp main.js index.html ../nikolakasev.github.io/football
