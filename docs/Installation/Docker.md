![docker logo](https://www.docker.com/sites/default/files/mono_horizontal_large.png)

# Installation on Docker

In order to install the program, you need to install Docker first.

## Installing Docker
To install Docker follow the [docker website](https://www.docker.com/community-edition) instructions and [post-install](https://docs.docker.com/engine/installation/linux/linux-postinstall/) instructions.
 
For Ubuntu:
```
sudo apt-get update
sudo apt-get install curl
sudo curl -sSL https://get.docker.com/ | sh
```
For Windows: follow the steps as described [here](https://github.com/biocatiit/musclex/issues/4)

For Mac: follow the steps as described [here](https://docs.docker.com/docker-for-mac/install/)

For Debian: follow the steps as described [here](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-debian-10)

## Create docker group and add a user
You need to add a user to docker group in order to give docker run permission to the user
```
sudo groupadd docker                
sudo usermod -aG docker [username] # replace [username] with your user name
```
## Installing and updating Muscle X program suite
You need to have the muscle.sh script available in order to update, and run a Muscle X program on docker. You can download muscle.sh by
```
wget https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh && chmod +x musclex.sh
```
or by
```
curl https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh -o musclex.sh
&& chmod +x musclex.sh
```
If you do not have `wget` or `curl` installed you can download the script directly at the following [link](https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh) and saving into a file. After saving the file make it executable (e.g. using: `chmod +x musclex.sh`).


## Running a Muscle X program
Running the program by running musclex.sh following by a program shortcut
```
./musclex.sh [program shortcut]
```
For example, run this command to run Equator
```
./musclex.sh eq
```
If you are using ssh to log into a remote server to run musclex there use the following instead
```
./musclex.sh -s eq
```
To see previous versions
```
./musclex.sh -l
```
To run a specific version
```
./musclex.sh -v 1-14.11 eq
```
To update musclex
```
./musclex.sh -u
```
To see all options
```
./musclex.sh -h
```



## Running a Muscle X program by building Docker image
Use this to build your own docker image and run musclex program on it. This would avoid any permission issues on the files created by the program. Download the following files and cd into the folder with these files.
```
musclex-uid.sh
dockerfile-uid
```
Run the following command to build the docker image,
```
./musclex-uid.sh -b
```
After successfully building the image, run musclex using the following command
```
./musclex-uid.sh [program shortcut]
```
For example, run this command to run Equator
```
./musclex-uid.sh eq
```
If you are using ssh to log into a remote server to run musclex there use the following instead
```
./musclex-uid.sh -s eq
```
To see previous versions
```
./musclex-uid.sh -l
```
To run a specific version
```
./musclex-uid.sh -v 1-14.11 eq
```
To update musclex
```
./musclex-uid.sh -u
```
To see all options
```
./musclex-uid.sh -h
```

