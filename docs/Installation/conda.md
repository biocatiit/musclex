## Install with `conda`

This guide covers installation of MuscleX using the Conda package manager, available through [Anaconda](https://www.anaconda.com/) or [Miniconda](https://docs.conda.io/en/latest/miniconda.html).



## Prerequisites

MuscleX is **tested with Python 3.10**, but should work with **Python 3.8 through 3.12**.



### Install Anaconda or Miniconda

- Install [Anaconda](https://www.anaconda.com/) or [Miniconda](https://docs.conda.io/en/latest/miniconda.html).

- After installing Miniconda on **Linux** or **macOS**, you may need to run:

  ```bash
  source ~/.bashrc   # or source ~/.bash_profile
  ```

  Then reopen your terminal to ensure the `conda` command is available.

- On **Windows**, open the "Anaconda Prompt" from the Start Menu to access `conda`.



### Check Your Python Version

After installing Conda, check your Python version to ensure it's within the supported range (3.8–3.12):

- **Linux/macOS**:

  ```bash
  python3 --version
  ```

- **Windows (Anaconda Prompt)**:

  ```cmd
  python --version
  ```

If your version is outside the supported range, continue with the steps below to create a new Conda environment with Python 3.10.



### Configure Channels (if needed)

To ensure access to all dependencies, you may want to explicitly add:

```bash
conda config --add channels conda-forge
conda config --add channels bioconda
```



### Create and Activate a Conda Environment

> **Note:** Only use a Conda-managed environment when installing via Conda. If you're using `pip`, use `venv` instead.

```bash
conda create -n musclex python=3.10
conda activate musclex
```

Using a virtual environment is recommended to avoid conflicts with system packages.



## Install MuscleX

### Install Latest Stable Release

```bash
conda install -c conda-forge musclex
```

### Install a Specific Version (e.g., v1.24.0)

```bash
conda install -c conda-forge musclex=1.24.0
```

> Beta and pre-release versions are **not available** via Conda as it does **not support installing directly from GitHub**. If you need a beta or development version, use the pip method instead.



## Verifying Installation

To confirm that MuscleX was installed correctly:

```bash
conda list musclex
```



## Running MuscleX

Ensure your environment is activated:

```bash
conda activate musclex
```

Then run a module:

```bash
musclex xv
```

To deactivate:

```bash
conda deactivate
```



## Troubleshooting

- **Missing GUI**:
   If you encounter GUI errors (e.g., related to PyQt or Qt platform plugins), install the required GUI toolkit:

  ```bash
  conda install pyqt
  ```

For additional help, visit the [GitHub Issues](https://github.com/biocatiit/musclex/issues) page.
