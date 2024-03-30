
![Conda](https://upload.wikimedia.org/wikipedia/commons/e/ea/Conda_logo.svg)

# Installing MuscleX using Conda

Conda is a powerful package manager and environment management system that allows you to install, run, and update packages and their dependencies. Below are the instructions to install `MuscleX` using Conda for Windows, macOS, and Linux.

## Prerequisites

Ensure you have Conda installed on your system. If not, install Miniconda from [Miniconda's official website](https://docs.conda.io/en/latest/miniconda.html), which is a minimal installer for Conda.

## Installation Instructions

### Windows

1. **Open Anaconda Prompt**: Search for "Anaconda Prompt" in the Start menu and open it.

2. **Create a New Conda Environment**: It's recommended to install `MuscleX` in a new environment to avoid conflicts with other packages.
   ```bash
   conda create --name musclex-env python=3.10
   ```
   The above command creates a new environment named `musclex-env` with Python 3.10. Replace `3.10` with the required Python version for `MuscleX`.

3. **Activate the Environment**:
   ```bash
   conda activate musclex-env
   ```

4. **Add fastai Channel**:
   ```bash
   conda config --add channels fastai
   ```
   This step is necessary to access the `fastai` channel that hosts a dependency for `MuscleX`.

5. **Install MuscleX**:
   ```bash
   conda install biocat_iit::musclex
   ```

### macOS and Linux

1. **Open Terminal**: Use Terminal on macOS or your preferred terminal emulator on Linux.

2. **Create a New Conda Environment**:
   ```bash
   conda create --name musclex-env python=3.10
   ```
   As with Windows, the above command creates a new environment named `musclex-env` with Python 3.10. Replace `3.10` with the required Python version for `MuscleX`.

3. **Activate the Environment**:
   ```bash
   conda activate musclex-env
   ```

4. **Add fastai Channel**:
   ```bash
   conda config --add channels fastai
   ```
   This step is necessary to access the `fastai` channel that hosts a dependency for `MuscleX`.

5. **Install MuscleX**:
   ```bash
   conda install biocat_iit::musclex
   ```

## Post-Installation

After installation, you can verify that `MuscleX` has been successfully installed by checking the list of installed packages:
```bash
conda list musclex
```

## Troubleshooting

If you encounter any issues during the installation process, ensure that you are connected to the internet, have permissions to install software on your system, and the Conda channels are accessible. 