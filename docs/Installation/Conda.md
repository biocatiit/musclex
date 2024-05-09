
![Conda](https://upload.wikimedia.org/wikipedia/commons/e/ea/Conda_logo.svg)

# Installation by Conda

Conda is a powerful package manager and environment management system that simplifies the process of installing, running, and updating packages and their dependencies. This guide provides instructions for installing `MuscleX` using Conda on Windows, macOS, and Linux.

## Before You Begin: Prerequisites

Before proceeding with the installation of `MuscleX` via Conda, **please ensure you have followed our [Prerequisites: setting up a conda virtual environment](Prerequisites.md)**. This guide will help you set up a Conda environment, crucial for a smooth installation process and avoiding conflicts with other packages on your system. The guide also explains the advantages of using Conda for managing virtual environments, especially for specifying different Python versions independent of the system interpreter.

## Installation Instructions

### For Windows Users

We assume that you have already installed Miniconda or Anaconda on your Windows system. If not, please refer to the [Miniconda installation guide](https://docs.conda.io/en/latest/miniconda.html) for detailed instructions.

1. **Open Anaconda Prompt**: Search for "Anaconda Prompt" in the Start menu and open it.

2. **Create a New Conda Environment**: It's strongly recommended to install `MuscleX` in a new, isolated environment to avoid conflicts with other packages.
   ```bash
   conda create --name musclex-env python=3.10
   ```
   This command creates a new environment named `musclex-env` with Python 3.10. You can replace `3.10` with the specific Python version required for `MuscleX`.

3. **Activate the Environment**:
   ```bash
   conda activate musclex-env
   ```

4. **Add fastai Channel** (Necessary for Dependencies):
   ```bash
   conda config --add channels fastai
   conda config --add channels conda-forge 
   ```
   This step ensures you can access the `fastai` channel that hosts a dependency required by `MuscleX` (opencv-python-headless) which is not available in the default channels. The `conda-forge` channel is also added to ensure you have access to a wide range of packages.

5. **Install MuscleX**:
   ```bash
   conda install --channel "biocat_IIT" musclex
   ```

### For macOS and Linux Users

The steps for macOS and Linux are similar to those for Windows. 
Ensure you have miniconda or anaconda installed on your system before proceeding. Refer to the [Miniconda installation guide](https://docs.conda.io/en/latest/miniconda.html) if you haven't installed it yet.

If this is your first time using Conda, you may need to initialize the shell by running:
```bash
conda init
```

## Post-Installation

To confirm that `MuscleX` has been installed successfully, you can check the list of installed packages with:
```bash
conda list musclex
```

## Troubleshooting

If you encounter any issues during the installation, please ensure that:
- You are connected to the internet.
- You have the necessary permissions to install software on your system.

Refer back to the [Prerequisite Guide](Prerequisites.md) for additional troubleshooting tips related to setting up and managing your Conda environment.
