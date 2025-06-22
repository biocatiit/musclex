Installing MuscleX
==================

MuscleX can be installed using several methods depending on your platform, technical comfort, and goals. This section provides an overview of the recommended approaches and includes platform-specific guides for each method.

Available Installation Methods
------------------------------

.. list-table::
   :header-rows: 1
   :widths: 10 30 30 30

   * - Method
     - Best For
     - Pros
     - Cons
   * - Pip
     - Most users on Linux/Mac/Windows with Python
     - Flexible, easy to update, virtualenv friendly
     - May require dev tools or compilers
   * - Conda
     - Users with Anaconda/Miniconda
     - Simple dependency management
     - Larger install base
   * - Docker
     - Developers or reproducible research environments
     - Fully isolated and portable
     - Requires Docker; GUI setup needed
   * - Installers
     - Non-technical users on Windows/macOS
     - Easy setup
     - Less control, not scriptable

Suggested Installation Strategy
-------------------------------

1. **Try `pip` (recommended for most Python users)**: Use this if you're comfortable with Python virtual environments and want the latest or specific versions.
2. **Use `conda`** if you prefer dependency resolution handled for you.
3. **Use Docker** for a fully isolated, repeatable environment.
4. **Use the Windows or Mac installer** if you are a non-technical user and just want a working GUI.

Contents
--------

The following guides describe how to install MuscleX using each method:

.. toctree::
   :maxdepth: 1

   pip
   conda
   docker
   installer
   troubleshooting
   testing
   developers
   advanced-testing

