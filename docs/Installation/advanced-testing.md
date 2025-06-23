# Advanced Testing for MuscleX

This guide is for developers who want to run detailed tests on MuscleX using the source repository.



## Testing Methodology

Tests reside in `musclex/tests/`. The module testing framework has two modes:

- **`testrecord`**: generates expected output and serializes it to Pickle files
- **`testverify`**: runs the same input again, serializes the output, and compares it to `testrecord`



## Running testrecord

```bash
cd musclex/tests/
python test_utils.py testrecord
```

Pickle files are saved under `<module_name>/test_pickles`.



## Running testverify

```bash
python test_utils.py testverify
```

Temporary results are stored in `tmp_verify_<settings>` and deleted after testing. Set `keeppickles=True` to retain them.



## Testing Data

Located in `musclex/tests/test_images/`. Outputs are compared to Pickle files to confirm correctness.



## Global Testing Module

Scripts used:

- `musclex/musclex_tester.sh`
- `musclex/musclex_headless_generator.sh`
- `musclex/musclex_headless_compare.sh`

These verify reproducibility and GUI/headless consistency.



## Unit Test Suite

Located in `musclex/tests/module_test.py`.

### Run all tests:

```bash
python test_utils.py testverify
```



### Test Examples

These tests are run using Python's `unittest` module. The configuration dictionaries shown in Python blocks are defined **inside the test scripts** and do **not** need to be passed manually. You can execute each test using the accompanying bash command.

Some tests (like `testHDFRead`, `testGPUIntegratePyFAI`, and `testOpenCLDevice`) do **not** require configuration settings.

**Equator Image:**

Python test settings:

```python
settingsA = {
  "left_sigmac": 1.0,
  "right_sigmac": 1.0,
  "orientation_model": 0,
  "nPeaks": 2,
  "model": "Gaussian",
  "isSkeletal": True,
  "mask_thres": -1.0,
  "90rotation": False,
  "blank_mask": False
}
```

Run with:

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testEquatorImage
```

**Quadrant Folder:**

Python test settings:

```python
settingsQF = {
  'bgsub': 'None',
  'sigmoid': 0.0,
  'no_cache': True,
  'orientation_model': 0
}
```

Run with:

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testQuadrantFolder
```

**Diffraction Centroids:**

Python test settings:

```python
settingsDC = {
  'orientation_model': 0,
  '90rotation': False,
  'no_cache': True
}
```

Run with:

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testDiffractionCentroids
```

**Projection Traces:**

Python test settings:

```python
settingsPT = {
  'boxes': {'box1': ((200, 800), (500, 600))},
  'bgsubs': {'box1': 0},
  'types': {'box1': 'h'},
  'peaks': {'box1': [100]},
  'bgsub': 'None',
  'sigmoid': 0.0,
  'no_cache': True,
  'orientation_model': 0
}
```

Run with:

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testProjectionTraces
```

**Scanning Diffraction:**

Python test settings:

```python
settings = {}
```

Run with:

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testScanningDiffraction
```

**HDF Read:**

No settings required.

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testHDFRead
```

**pyFAI Integration:**

No settings required.

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testGPUIntegratePyFAI
```

**GPU Device:**

No settings required.

```bash
python musclex_test/tests/test_suite.py MuscleXTest.testOpenCLDevice
```
