#!/bin/bash
sudo apt-get update
sudp apt-get install curl
sudo curl -sSL https://get.docker.com/ | sh
sudo docker pull biocat/biocat
sudo cp biocat.sh /usr/local/bin/biocat
sudo chmod +x /usr/local/bin/biocat
