#!/bin/bash
xhost local:root
runapp=true
printinfo=false

if [ -z "$1" ]; then
    runapp=false
    printinfo=true
elif [ "$1" == "bm" ]; then
	echo "bio_muscle is running"
elif [ "$1" == "dc" ]; then
	echo "diffraction_centroids is running"
elif [ "$1" == "cp" ]; then
	echo "Circular Projection is running"
elif [ "$1" == "qf" ]; then
	echo "Quadrant Folding is running"
elif [ "$1" == "ddf" ]; then
	echo "DDF Processor is running"
elif [ "$1" == "update" ]; then
    echo "biocat is updating"
    runapp=false
    docker rmi $(docker images | grep 'biocat/musclex')
    docker pull biocat/biocat
fi

if [ "$runapp" = true ]; then
	docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix -v $(pwd):$(pwd) -w $(pwd) --privileged -v /home:/home -e DISPLAY=unix$DISPLAY -e PYTHONUNBUFFERED=0 biocat/musclex musclex $1
elif [ "$printinfo" = true ]; then
	echo "Please specify the program shortcut that you want to run"
	echo ""
	echo "  $ ./muscle.sh [--program]"
	echo ""
    echo "          bm - Bio Muscle "
    echo "          qf - Quadrant Folding"
    echo "          cp - Circular Projection"
    echo "          dc - Diffraction Centroids"
    echo "          ddf - DDF Processor"
    echo ""
    echo " or"
    echo ""
    echo "  $ ./muscle.sh update"
    echo ""
    echo "          to update the programs"
fi
