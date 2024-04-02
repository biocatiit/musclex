
# Prerequisite Guide: Setting Up Virtual Environments for MuscleX Installation

Before installing MuscleX via Conda, pip, or directly from GitHub, it's crucial to establish a virtual environment. This step ensures your project dependencies are managed in an isolated space, preventing conflicts. This guide emphasizes **Conda** as the recommended tool for environment management, with **venv** presented as an alternative option.

## ⚠️ Important Consideration

**Conda is highly recommended** for creating virtual environments, especially since it allows for the specification of Python versions within each environment, **independent of the system's Python interpreter**. This flexibility is particularly beneficial for projects requiring a specific Python version or those aiming to avoid interference with system-wide Python settings.

## Why Use a Virtual Environment?

A virtual environment is necessary for installations involving:
- **Conda**: When installing MuscleX from a Conda channel.
- **Pip**: For installations via the Python Package Index (PyPI) or from GitHub.
- **GitHub**: When installing the latest development version from the GitHub repository.

Using a virtual environment ensures a clean, controlled setup, isolating MuscleX's dependencies from other Python projects on your system.

## Recommended: Conda Environment Management

Conda is an all-encompassing package and environment management system, ideal for handling complex dependencies across multiple platforms.

### Advantages of Conda:

- **Cross-Platform Compatibility**: Functions on Windows, macOS, and Linux.
- **Advanced Dependency Management**: Excellently resolves complex dependencies to avert compatibility issues.
- **Environment Isolation**: Safeguards your project by preventing cross-environment interference.

### Setting Up Conda:

1. **Install Miniconda**: Refer to the [Miniconda installation guide](https://docs.conda.io/en/latest/miniconda.html) for your operating system.

2. **Create a New Environment**: For an environment named `myenv` with Python 3.10, use:

   ```bash
   conda create -n myenv python=3.10
   ```

3. **Activate the Environment**:

   ```bash
   conda activate myenv
   ```

Given its extensive features and user-friendly nature, Conda is the preferred choice for most users.

## Alternative: venv for Python Projects

venv is a viable alternative, provided with Python 3.3 and later. 

## ⚠️ Important Consideration 
Only use venv if you are installing MuscleX via pip or GitHub, because installing MuscleX via Conda requires a Conda environment. If you are using Conda, please refer to the Conda environment setup instructions above.

### Advantages of venv:

- **Built-In with Python**: No need for additional installations.

### Setting Up venv:

1. **Verify Python Version**: Ensure you have Python 3.3 or newer by running `python --version`.

2. **Create a New Environment**: Within your project directory:

   ```bash
   python -m venv myenv
   ```

3. **Activate the Environment**:

   - **Windows**:

     ```cmd
     myenv\Scripts\activate
     ```

   - **macOS and Linux**:

     ```bash
     source myenv/bin/activate
     ```
