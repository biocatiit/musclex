#!/bin/bash
xhost local:root
runapp=true

if [ "$1" == "update" ]; then
    echo "Muscle X on Docker is updating..."
    runapp=false
    docker rmi $(docker images | grep 'biocat/musclex')
    docker pull biocat/musclex
fi

if [ "$runapp" = true ]; then
	docker run --rm --net=host --env="DISPLAY" --volume="$HOME/.Xauthority:/root/.Xauthority:rw" -v /tmp/.X11-unix:/tmp/.X11-unix -v $(pwd):$(pwd) -w $(pwd) --privileged -v /home:/home -e DISPLAY=$DISPLAY -e PYTHONUNBUFFERED=0 biocat/musclex musclex $1
fi
