![docker logo](https://www.docker.com/wp-content/uploads/2022/03/horizontal-logo-monochromatic-white.png)

# Installation on Docker

In order to install the program, you need to install Docker first.

## Installing Docker (rootless mode)

MuscleX is often creating folders during its use in order to store the results. With the normal version of Docker, you will need to be a root user to access those files.
However, since Docker v20.10, it is possible to use 'rootless' Docker. We recommend using this version of Docker to facilitate the access to MuscleX's results.

Follow the [rootless docker](https://docs.docker.com/engine/security/rootless/) instructions and [post-install](https://docs.docker.com/engine/installation/linux/linux-postinstall/) instructions.

For Ubuntu:
```
sudo apt-get update
sudo apt-get install curl
curl -fsSL https://get.docker.com/rootless | sh
```

Make sure the following environment variables are set (use `echo $PATH` and `echo $DOCKER_HOST`). If this is not the case, execute this:
```
export PATH=/home/[user]/bin:$PATH
export DOCKER_HOST=unix:///run/user/1000/docker.sock
```

For other distributions, refer to the [rootless docker website](https://docs.docker.com/engine/security/rootless/).

If the installation fails with `Aborting because rootful Docker (/var/run/docker.sock) is running and accessible.`, run the following command `sudo systemctl disable --now docker.service docker.socket` then reboot your system before trying the installation again.

If the results folders are still under root rights, it might be because you are still using the "rootful" version of Docker, run `docker context use rootless`.

## Create docker group and add a user
You need to add a user to docker group in order to give docker run permission to the user  
```
sudo groupadd docker                
sudo usermod -aG docker [username] # replace [username] with your user name
```  
Note: You need to log out and log in again for these settings to take effect.  

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
