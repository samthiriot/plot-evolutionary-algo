#!/bin/sh

VERSION=`date +"%Y-%m-%d-%H-%M"`

FILENAME=shiny-app-$VERSION

WD=`pwd`

mkdir -p /tmp/build-shiny-app

cp LICENSE plot-evolution/app.R /tmp/build-shiny-app/

cd /tmp/build-shiny-app

zip -r9 $WD/$FILENAME LICENSE app.R 

cd $WD 

rm -rf /tmp/build-shiny-app

ls -lah $FILENAME.zip

echo "also mind updating https://www.shinyapps.io/admin/#/dashboard"
