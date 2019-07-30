#!/bin/bash

version=""
sshoptions=""
while getopts ":ulhsv:" opt; do
  case ${opt} in
    u ) 
      echo "Muscle X on Docker is updating..."
      #docker rmi $(docker images | grep 'biocat/musclex')
      docker pull biocat/musclex
      exit
      ;;
    l ) 
      curl 'https://registry.hub.docker.com/v2/repositories/biocat/musclex/tags/' | grep -P -o '\"name\": \"[^\"]*\"'
      exit
      ;;
    h ) 
      echo ""
      echo "Usage: $(basename $0) [-h] [-u] [-l] [-v version] [-s] program"
      echo ""
      echo "  h = help" 
      echo "  u = update" 
      echo "  l = list versions" 
      echo "  s = ssh permissions" 
      echo "  v = select version (e.g. -v 1-14.4)" 
      echo ""
      echo "E.g.:"
      echo "  $(basename $0) -l"
      echo "  $(basename $0) -v 1-14.4 eq"
      echo "  $(basename $0) -v 1-14.11 -s eq"
      echo ""
      exit
      ;;
    s )
      sshoptions="--net=host --env=\"DISPLAY\" --volume=$HOME/.Xauthority:/root/.Xauthority:rw"
      ;;
    v )
      version=":$OPTARG"
      ;;
  esac
done
shift "$(($OPTIND -1))"

docker run --rm $sshoptions -v /tmp/.X11-unix:/tmp/.X11-unix -v $(pwd):$(pwd) -w $(pwd) --privileged -v /home:/home -e DISPLAY=unix$DISPLAY -e PYTHONUNBUFFERED=0 biocat/musclex$version musclex $1


