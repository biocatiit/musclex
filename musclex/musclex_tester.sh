#!/usr/bin/bash

echo "Starting tests..."

mkdir -p tests/test_logs
LOG='tests/test_logs/test.log'

<<COMMENT
while true; do
	read -p 'Do you want to generate results before testing your software? It will take a few minutes. [y/n]' -r -n 1 yn
	echo
	case $yn in
		[Yy]* ) echo "Generating a headless instance..."; ./musclex_headless_generator.sh; ./musclex_headless_compare.sh; break;;
		[Nn]* ) break;;
		* ) echo "Please answer yes or no.";;
	esac
done
COMMENT

echo "Generating a headless instance..."

./musclex_headless_generator.sh
./musclex_headless_compare.sh

echo 'Comparing headless and correct instances...' | tee -a $LOG

### MAR images ###
## Analysis and comparison ##
# Equator headless test #
echo "
------------------------------------ MAR RESULTS ------------------------------------" | tee -a $LOG
echo "Comparing the results of Equator with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
  if ($1 == Y) {
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
  } 
}' "tests/testResults/MARimages/eq_results/summary2.csv" >> file1
	echo $line | awk -F',' -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
}' >> file11
done < tests/testImages/MARimages/eq_results/summary2.csv
diff --color file11 file1 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi
# Diffraction headless test #
echo "Comparing the results of Diffraction with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
  if ($1 == Y) {
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
  } 
}' "tests/testResults/MARimages/di_results/summary.csv" >> file2
	echo $line | awk -F',' -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
}' >> file22
done < tests/testImages/MARimages/di_results/summary.csv
diff -y --color file22 file2 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi
# Quadrant folder headless test #
echo "Comparing the results of Quandrant Folder with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" '{ if ( Y == $1 ) print $0 }' "tests/testResults/MARimages/qf_results/summary.csv" >> file3
	echo $line >> file33
done < tests/testImages/MARimages/qf_results/summary.csv
diff -y --color file33 file3 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi

rm -f file1 file11 file2 file22 file3 file33 filename res

### EIGER images ###
## Analysis and comparison ##
# Equator headless test #
echo "
------------------------------------ EIGER RESULTS ------------------------------------" | tee -a $LOG
echo "Comparing the results of Equator with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
  if ($1 == Y) {
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
  } 
}' "tests/testResults/EIGERimages/eq_results/summary2.csv" >> file1
	echo $line | awk -F',' -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
}' >> file11
done < tests/testImages/EIGERimages/eq_results/summary2.csv
diff -y --color file11 file1 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi
# Diffraction headless test #
echo "Comparing the results of Diffraction with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
  if ($1 == Y) {
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
  } 
}' "tests/testResults/EIGERimages/di_results/summary.csv" >> file2
	echo $line | awk -F',' -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
}' >> file22
done < tests/testImages/EIGERimages/di_results/summary.csv
diff -y --color file22 file2 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi
# Quadrant folder headless test #
echo "Comparing the results of Quandrant Folder with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" '{ if ( Y == $1 ) print $0 }' "tests/testResults/EIGERimages/qf_results/summary.csv" >> file3
	echo $line >> file33
done < tests/testImages/EIGERimages/qf_results/summary.csv
diff -y --color file33 file3 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi

rm -f file1 file11 file2 file22 file3 file33 filename res

### Pilatus 1M images ###
## Analysis and comparison ##
# Equator headless test #
echo "
------------------------------------ PILATUS 1M RESULTS ------------------------------------" | tee -a $LOG
echo "Comparing the results of Equator with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
  if ($1 == Y) {
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
  } 
}' "tests/testResults/PILATUSimages/eq_results/summary2.csv" >> file1
	echo $line | awk -F',' -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
}' >> file11
done < tests/testImages/PILATUSimages/eq_results/summary2.csv
diff -y --color file11 file1 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi
# Diffraction headless test #
echo "Comparing the results of Diffraction with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
  if ($1 == Y) {
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
  } 
}' "tests/testResults/PILATUSimages/di_results/summary.csv" >> file2
	echo $line | awk -F',' -vOFS=',' '
function foo(str) {
  if(match(str, /[0-9]+\.[0-9]+/)) {
    gsub(/[0-9]+\.[0-9]+/, sprintf("%.4f",str), str)
  } 
  return str;
} 
{
    for (i=1; i<=NF; i++) 
    	printf "%s%s", foo($i),(i<NF?OFS:ORS)
}' >> file22
done < tests/testImages/PILATUSimages/di_results/summary.csv
diff -y --color file22 file2 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi
# Quadrant folder headless test #
echo "Comparing the results of Quandrant Folder with a set of correct results..." | tee -a $LOG
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" '{ if ( Y == $1 ) print $0 }' "tests/testResults/PILATUSimages/qf_results/summary.csv" >> file3
	echo $line >> file33
done < tests/testImages/PILATUSimages/qf_results/summary.csv
diff -y --color file33 file3 > res
if grep -e'-,-' -q res
then
	echo 'Failed to produce the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'tif,-,-' res | tee -a $LOG
	echo -e '=== \033[0;31m[ERROR]\033[0m' | tee -a $LOG
fi
if grep -e'|\|<\|>' -q res
then
	echo 'Mismatches between the following files:' | tee -a $LOG
	echo '===' | tee -a $LOG
	grep -e'|\|<\|>' res | tee -a $LOG
	echo -e '=== \033[0;31m[FAIL]\033[0m' | tee -a $LOG
else
	echo -e "\033[0;32m[PASS]\033[0m" | tee -a $LOG
fi

rm -f file1 file11 file2 file22 file3 file33 filename res

echo "
Done.
"
<<COMMENT
while true; do
	read -p 'Do you want to clean the results generated during the testing? [y/n]' -r -n 1 yn
	echo
	case $yn in
		[Yy]* ) echo "Deleting generated files...";
		rm -r testImages/EIGERimages/eq_*;
		rm -r testImages/EIGERimages/qf_*;
		rm -r testImages/EIGERimages/di_*;
		rm -r testImages/MARimages/eq_*;
		rm -r testImages/MARimages/qf_*;
		rm -r testImages/MARimages/di_*;
		rm -r testImages/PILATUSimages/eq_*;
		rm -r testImages/PILATUSimages/qf_*;
		rm -r testImages/PILATUSimages/di_*;
		 break;;
		[Nn]* ) break;;
		* ) echo "Please answer yes or no.";;
	esac
done
COMMENT

echo "Deleting generated files..."
rm -r tests/testImages/EIGERimages/eq_*
rm -r tests/testImages/EIGERimages/qf_*
rm -r tests/testImages/EIGERimages/di_*
rm -r tests/testImages/MARimages/eq_*
rm -r tests/testImages/MARimages/qf_*
rm -r tests/testImages/MARimages/di_*
rm -r tests/testImages/PILATUSimages/eq_*
rm -r tests/testImages/PILATUSimages/qf_*
rm -r tests/testImages/PILATUSimages/di_*
rm -r tests/testImages/EIGERimages/failedcases.txt
rm -r tests/testImages/MARimages/failedcases.txt
rm -r tests/testImages/PILATUSimages/failedcases.txt

echo "Done."

