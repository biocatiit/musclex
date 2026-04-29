"""
Fitting A/B test framework for ProjectionProcessor.

Goal
----
Compare alternative non-linear fitting backends (lmfit/leastsq, lmfit/TRF,
scipy, VarPro, iminuit, ...) against the current baseline on *real* fit
inputs captured during normal MuscleX runs.

Key components
--------------
- ``adapters.base``  : ``FitCase``, ``FitResult``, ``FitterAdapter`` interface.
- ``adapters.*``     : concrete adapters (one per algorithm under test).
- ``capture.recorder``: hooks into ``ProjectionProcessor.fitModel`` to dump
  every fit's inputs + reference output to a pickle file.
- ``runner``         : replays cases x adapters, collects per-fit metrics.
- ``metrics``        : diff/regression metrics (parameter deltas, chi2, etc).

Quick start
-----------
1. Capture cases by setting an env flag and running tests::

       export MUSCLEX_CAPTURE_FITS=1
       export MUSCLEX_CAPTURE_DIR=/tmp/musclex_fit_cases
       python -m unittest musclex.tests.musclex_tester

2. Replay them through baseline and any new adapter::

       python -m musclex.tests.fitting_ab.runner \\
           --cases /tmp/musclex_fit_cases \\
           --adapters lmfit-baseline lmfit-trf lmfit-weighted \\
           --report /tmp/ab_report.csv
"""
