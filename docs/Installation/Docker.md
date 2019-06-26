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
For Windows: you might need to do more steps as described [here](https://github.com/biocatiit/musclex/issues/4)

## Create docker group and add a user
You need to add a user to docker group in order to give docker run permission to the user
```
sudo groupadd docker                
sudo usermod -aG docker [username] # replace [username] with your user name
```
## Installing and updating Muscle X program suite
You need to have the muscle.sh script available in order to update, and run a Muscle X program on docker. You can download muscle.sh and the latest docker image by
```
wget https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh && chmod +x musclex.sh
./musclex.sh update
```
If you are using ssh to log into a remote server to run musclex there use the following instead
```
wget https://raw.githubusercontent.com/biocatiit/musclex/master/musclex-ssh.sh && chmod +x musclex-ssh.sh
./musclex.sh update
```


## Running a Muscle X program
Running the program by running musclex.sh following by a program shortcut
```
./musclex.sh [program shortcut]
```
For example, run this command to run Bio-Muscle
```
./musclex.sh bm
```
If you are using ssh to log into a remote server to run musclex there use the following instead
```
./musclex-ssh.sh bm
```

