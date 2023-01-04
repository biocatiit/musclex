# How to create MuscleX reports

Regroups all the tests concerning the speed, coverage and memory usage of MuscleX.

## Memory usage report

To use memory profiler:
	mprof run musclex qf

To display the result:
	mprof plot

To have the details, run:
	python -m memory_profiler example.py


## Time per call reports

To profile calls, add in main.py by replacing what is in if __name__ == “__main__” by: 

import cProfile
cProfile.run('main(sys.argv)', 'output.dat')
import pstats
from pstats import SortKey
with open('output_time.txt', 'w') as f:
	p = pstats.Stats('output.dat', stream=f)
	p.sort_stats('time').print_stats()
with open('output_calls.txt', 'w') as f:
    p = pstats.Stats('output.dat', stream=f)
    p.sort_stats('calls').print_stats()

Run the program:
    python main.py di -h -i ~/data/EIGER_testImages/17ER_190_data_000001_0001.tif -d

## Coverage reports

In most files, the imports are written in 2 different ways to satisfy how coverage work. This is why there are "try" and "except" on the imports of most of the files.

To use coverage.py, go at the root of the code, where "main.py" is, and run:
    coverage run -m unittest tests/musclex_tester.py


To generate the file:
    coverage html -i

## CPU usage

Run:
    htop

Then run MuscleX.

## GPU usgae

Run:
    nvtop

Then run MuscleX.

