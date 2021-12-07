#!/usr/bin/env bash

# 0. Source this file via `source $APPS_MY_PORTFOLIO_ROOT/utils/prep-logo-images.sh`

# 1. Some images you may need to remove the background of.
#In this case you:
# - mouse over the single image layer in bottom right corner; right click add alpha layer
# - use the magic-wand-tool to select the area to remove
# - hit delete to remove area
# - export as png

# 2. resize image via the image-magick command: `convert `
# resize the image to a max height of 200px, ensure centered
# `convert $1 -resize x200 -gravity center $2`

function rz200() {
    convert "$1" -resize x200 -gravity center "$2"
}

function crop-centered() {
    convert "$1" -gravity center -crop $3+0+0 +repage "$2"
}

# 3. Move image to logo directory
function mv-logo() {
    mv "$1" "$APPS_MY_PORTFOLIO_ROOT/static/logos"
}
