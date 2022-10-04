#!/usr/bin/bash

DIR=$( pwd; )

mkdir $DIR/tests/tmp
mkdir $DIR/tests/tmp/PILATUSimages
mkdir $DIR/tests/tmp/EIGERimages
mkdir $DIR/tests/tmp/MARimages
mv -f $DIR/tests/testImages/PILATUSimages/eq_* $DIR/tests/tmp/PILATUSimages
mv -f $DIR/tests/testImages/PILATUSimages/qf_* $DIR/tests/tmp/PILATUSimages
mv -f $DIR/tests/testImages/PILATUSimages/cp_* $DIR/tests/tmp/PILATUSimages
mv -f $DIR/tests/testImages/EIGERimages/eq_* $DIR/tests/tmp/EIGERimages
mv -f $DIR/tests/testImages/EIGERimages/qf_* $DIR/tests/tmp/EIGERimages
mv -f $DIR/tests/testImages/EIGERimages/cp_* $DIR/tests/tmp/EIGERimages
mv -f $DIR/tests/testImages/MARimages/eq_* $DIR/tests/tmp/MARimages
mv -f $DIR/tests/testImages/MARimages/qf_* $DIR/tests/tmp/MARimages
mv -f $DIR/tests/testImages/MARimages/cp_* $DIR/tests/tmp/MARimages

echo "Generating a second headless instance..."

./musclex_headless_generator.sh

LOG='tests/test_logs/summary_test.log'
echo 'Summary of Test Results' >> $LOG

echo "Comparing two headless instances..."

### MAR images ###
## Analysis and comparison ##
# Equator headless test #
echo "
------------------------------------ MAR RESULTS ------------------------------------"
echo "Comparing the results of Equator with another instance of headless..."
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
}' "$DIR/tests/tmp/MARimages/eq_results/summary2.csv" >> file1
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
done < $DIR/tests/testImages/MARimages/eq_results/summary2.csv
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
echo "Comparing the results of Diffraction with another instance of headless..."
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
}' "$DIR/tests/tmp/MARimages/cp_results/summary.csv" >> file2
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
done < $DIR/tests/testImages/MARimages/cp_results/summary.csv
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
echo "Comparing the results of Quandrant Folder with another instance of headless..."
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" '{ if ( Y == $1 ) print $0 }' "$DIR/tests/tmp/MARimages/qf_results/summary.csv" >> file3
	echo $line >> file33
done < $DIR/tests/testImages/MARimages/qf_results/summary.csv
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
# Equator headless test (without calibration) #
echo "
------------------------------------ EIGER RESULTS ------------------------------------"
echo "Comparing the results of Equator with another instance of headless..."
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
}' "$DIR/tests/tmp/EIGERimages/eq_results/summary2.csv" >> file1
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
done < $DIR/tests/testImages/EIGERimages/eq_results/summary2.csv
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
# Diffraction headless test (without calibration) #
echo "Comparing the results of Diffraction with another instance of headless..."
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
}' "$DIR/tests/tmp/EIGERimages/cp_results/summary.csv" >> file2
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
done < $DIR/tests/testImages/EIGERimages/cp_results/summary.csv
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
echo "Comparing the results of Quandrant Folder with another instance of headless..."
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" '{ if ( Y == $1 ) print $0 }' "$DIR/tests/tmp/EIGERimages/qf_results/summary.csv" >> file3
	echo $line >> file33
done < $DIR/tests/testImages/EIGERimages/qf_results/summary.csv
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
------------------------------------ PILATUS 1M RESULTS ------------------------------------"
echo "Comparing the results of Equator with another instance of headless..."
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
}' "$DIR/tests/tmp/PILATUSimages/eq_results/summary2.csv" >> file1
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
done < $DIR/tests/testImages/PILATUSimages/eq_results/summary2.csv
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
echo "Comparing the results of Diffraction with another instance of headless..."
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
}' "$DIR/tests/tmp/PILATUSimages/cp_results/summary.csv" >> file2
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
done < $DIR/tests/testImages/PILATUSimages/cp_results/summary.csv
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
echo "Comparing the results of Quandrant Folder with another instance of headless..."
while read line
do
	name=$(cut -d, -f1 <<<"$line")
	awk -F',' -vY="$name" '{ if ( Y == $1 ) print $0 }' "$DIR/tests/tmp/PILATUSimages/qf_results/summary.csv" >> file3
	echo $line >> file33
done < $DIR/tests/testImages/PILATUSimages/qf_results/summary.csv
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
rm -r $DIR/tests/tmp

echo "
Done."
