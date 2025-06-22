## Tests Overview

This guide describes how to verify your MuscleX installation and use its testing framework to ensure consistent and reproducible results across devices and versions. For most users, it is not necessary to continue beyond the *Testing Methodology for Developers* section.



## Testing a MuscleX Installation

### GUI Testing

1. Launch the MuscleX GUI
2. Click **Run Tests** in the launcher window
3. Click **Run Global Tests** to run all module tests
4. For GPU-specific testing, click **Run GPU Test**
   - This verifies if `pyopencl` is installed and GPU access is available

### Command Line Testing

Run the following commands in a terminal:

- Run all module tests:

  ```bash
  musclex test_global
  ```

- Run detailed implementation tests:

  ```bash
  musclex test_impl
  ```

- Run GPU test only:

  ```bash
  musclex test_gpu
  ```

- Run environment test:

  ```bash
  musclex test_env
  ```

> `test_env` compares your current Python and dependency versions against a reference set saved in `musclex/environment_tester.sh`. Failing this test does not necessarily mean MuscleX won’t work.

For further help, open an issue on [GitHub](https://github.com/biocatiit/musclex/issues).