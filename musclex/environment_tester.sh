#!/usr/bin/bash

mkdir -p tests/test_logs
LOG="tests/test_logs/test.log"

echo "
PACKAGES AT THE RELEASE of v1.15.6
" | tee -a "$LOG"
PYVERS="Python 3.8.14" # This is a copy paste of the result of 'python --version' before making a new release
echo $PYVERS | tee -a "$LOG"
echo "
Package         Version
------------- - - -------"
echo "scikit-image      0.19.3
tifffile          2022.8.12
numpy             1.23.3
pandas            1.5.0rc0
scikit-learn      1.1.2
lmfit             1.0.3
fabio             0.14.0
h5py              3.7.0
scipy             1.9.1
matplotlib        3.5.3
Cython            0.29.32
opencv-python     4.1.2.30
pyFAI             0.21.3" | tee -a pack | tee -a $LOG # This is a copy paste of the result following before making a new release

echo "

PACKAGES INSTALLED ON YOUR COMPUTER
" | tee -a $LOG

VERS=$(python --version)
echo $VERS | tee -a $LOG
echo "
Package         Version" | tee -a $LOG
echo "------------- - - -------" | tee -a $LOG
echo "scikit-image
tifffile
numpy
pandas
scikit-learn
lmfit
ConfigParser
pillow
fabio
peakutils
h5py
scipy
matplotlib
musclex_ccp13
Cython
opencv-python
pyFAI" > reqs # This is a copy-paste of the file named 'requirements'
while read line
do
	pip list | grep $line | tee -a packages_installed | tee -a $LOG
done < reqs
rm reqs
echo

if [ "$VERS" != "$PYVERS" ]; then
	echo -e "\033[0;31m--- WARNING: You are using a different Python version, this may affect the stability of the software. Please use a version of Python 3.8\033[0m" | tee -a $LOG
fi

diff -y packages_installed pack > res
if grep -e'|\|<\|>' -q res
then
	echo -e '\033[0;31m--- WARNING: Mismatches between the following packages:\033[0m' | tee -a $LOG
	echo -e '\033[0;31mYour version ================================================= Version at release =====================\033[0m' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '\033[0;31m============================================================== ========================================
This may affect the stability of the software. Please use similar packages\033[0m
' | tee -a $LOG
else
	echo -e "\033[0;32m[THE PACKAGES ARE IDENTICAL]\033[0m
" | tee -a $LOG
fi

rm -f packages_installed pack res

