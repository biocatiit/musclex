#!/bin/bash
xhost local:root
CODE_PATH=/bio_muscle.py
runapp=true
printinfo=false

if [ -z "$1" ]; then
    runapp=false
    printinfo=true
elif [ "$1" == "bm" ]; then
	echo "bio_muscle is running"
	CODE_PATH=/bio_muscle.py
elif [ "$1" == "dc" ]; then
	echo "diffraction_centroids is running"
	CODE_PATH=/diffraction_centroids.py
elif [ "$1" == "cp" ]; then
	echo "Circular Projection is running"
	CODE_PATH=/circular_projection_v2.py
elif [ "$1" == "qf" ]; then
	echo "Quadrant Folding is running"
	CODE_PATH=/quadrant_folding.py
elif [ "$1" == "ddf" ]; then
	echo "DDF Processor is running"
	CODE_PATH=/ddf_processor.py
elif [ "$1" == "update" ]; then
    echo "biocat is updating"
    runapp=false
    docker rmi $(docker images | grep 'biocat/biocat')
    docker pull biocat/biocat
fi

if [ "$runapp" = true ]; then
	docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix -v $(pwd):$(pwd) -w $(pwd) --privileged -v /home:/home -e DISPLAY=unix$DISPLAY -e PYTHONUNBUFFERED=0 biocat/biocat python $CODE_PATH
elif [ "$printinfo" = true ]; then
	echo "Please specify the program shortcut that you want to run"
	echo ""
	echo "  $ biocat [--program]"
	echo ""
    echo "          bm - Bio Muscle "
    echo "          qf - Quadrant Folding"
    echo "          cp - Circular Projection"
    echo "          dc - Diffraction Centroids"
    echo "          ddf - DDF Processor"
    echo ""
    echo " or"
    echo ""
    echo "  $ biocat update"
    echo ""
    echo "          to update the programs"
fi
