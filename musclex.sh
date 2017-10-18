#!/bin/bash
xhost local:root
runapp=true

if [ "$1" == "update" ]; then
    echo "biocat is updating"
    runapp=false
    docker rmi $(docker images | grep 'biocat/musclex')
    docker pull biocat/biocat
fi

if [ "$runapp" = true ]; then
	docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix -v $(pwd):$(pwd) -w $(pwd) --privileged -v /home:/home -e DISPLAY=unix$DISPLAY -e PYTHONUNBUFFERED=0 biocat/musclex musclex $1
fi