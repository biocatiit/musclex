# Install via Docker

This guide explains how to run MuscleX using Docker containers. This method provides a consistent, portable environment for running MuscleX across different platforms.



### Prerequisites

You must have Docker installed and configured. Follow the appropriate instructions for your operating system.

#### **Linux (Ubuntu)**

```bash
sudo apt update
sudo apt install curl docker.io
```

For rootless Docker (recommended):

```bash
curl -fsSL https://get.docker.com/rootless | sh
```

Then set environment variables:

```bash
export PATH=/home/$USER/bin:$PATH
export DOCKER_HOST=unix:///run/user/1000/docker.sock
```

If you get an error like:

```
Aborting because rootful Docker (/var/run/docker.sock) is running and accessible.
```

Then run:

```bash
sudo systemctl disable --now docker.service docker.socket
reboot
```

Ensure you're using rootless Docker:

```bash
docker context use rootless
```

Add your user to the `docker` group:

```bash
sudo groupadd docker
sudo usermod -aG docker $USER
```

> Log out and log back in for this to take effect.

#### **macOS and Windows**

- Download and install Docker Desktop:
  - [macOS](https://docs.docker.com/docker-for-mac/install/)
  - [Windows](https://docs.docker.com/docker-for-windows/install/)
- Ensure Docker Desktop is running.
- For WSL2 (Windows), enable integration in Docker Desktop settings.



### Build and Run MuscleX Container

```bash
git clone https://github.com/biocatiit/musclex.git
cd musclex
docker build -t musclex .
docker run -it musclex
```

> To run GUI apps in Docker (Linux), you may need to forward X11.



### Optional: Use musclex.sh Script for Ease of Use

Download the helper script:

```bash
wget https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh && chmod +x musclex.sh
```

or

```bash
curl https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh -o musclex.sh && chmod +x musclex.sh
```

You can also download directly via browser from [this link](https://raw.githubusercontent.com/biocatiit/musclex/master/musclex.sh).



### Running a MuscleX Program

Run using:

```bash
./musclex.sh [program shortcut]
```

Example:

```bash
./musclex.sh eq
```

To run over SSH:

```bash
./musclex.sh -s eq
```



### Additional Commands

- **List versions**:

```bash
./musclex.sh -l
```

- **Run specific version**:

```bash
./musclex.sh -v 1-14.11 eq
```

- **Update MuscleX**:

```bash
./musclex.sh -u
```

- **View help**:

```bash
./musclex.sh -h
```



For more on running Docker containers and troubleshooting GUI forwarding, refer to the official [Docker documentation](https://docs.docker.com/).
