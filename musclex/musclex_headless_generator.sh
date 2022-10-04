#!/usr/bin/bash

DIR=$( pwd; )

LOG='tests/test_logs/summary_test.log'

### MAR images ###
## Creation of results ##
# Equator headless test #
echo "GENERATING EQUATOR ON MARimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/MARimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex eq -h -i $file -s "$DIR/tests/testImages/MARimages/eqsettings.json" -d > /dev/null
	let counter=counter+1
done
# Diffraction headless test #
echo "GENERATING DIFFRACTION ON MARimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/MARimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex di -h -i $file -s "$DIR/tests/testImages/MARimages/disettings.json" -d > /dev/null
	let counter=counter+1
done
# Quadrant folder headless test #
echo "GENERATING QUADRANT FOLDING ON MARimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/MARimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex qf -h -i $file -s "$DIR/tests/testImages/MARimages/qfsettings.json" -d > /dev/null
	let counter=counter+1
done

### EIGER images ###
## Creation of results ##
# Equator headless test (without calibration) #
echo "GENERATING EQUATOR ON EIGERimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/EIGERimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex eq -h -i $file -s "$DIR/tests/testImages/EIGERimages/eqsettings.json" -d > /dev/null
	let counter=counter+1
done
# Diffraction headless test (without calibration) #
echo "GENERATING DIFFRACTION ON EIGERimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/EIGERimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex di -h -i $file -s "$DIR/tests/testImages/EIGERimages/disettings.json" -d > /dev/null
	let counter=counter+1
done
# Quadrant folder headless test (without calibration) #
echo "GENERATING QUADRANT FOLDING ON EIGERimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/EIGERimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex qf -h -i $file -s "$DIR/tests/testImages/EIGERimages/qfsettings.json" -d > /dev/null
	let counter=counter+1
done

### Pilatus 1M images ###
## Creation of results ##
# Equator headless test #
echo "GENERATING EQUATOR ON PILATUSimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/PILATUSimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex eq -h -i $file -s "$DIR/tests/testImages/PILATUSimages/eqsettings.json" -d > /dev/null
	let counter=counter+1
done
# Diffraction headless test #
echo "GENERATING DIFFRACTION ON PILATUSimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/PILATUSimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex di -h -i $file -s "$DIR/tests/testImages/PILATUSimages/disettings.json" -d > /dev/null
	let counter=counter+1
done
# Quadrant folder headless test #
echo "GENERATING QUADRANT FOLDING ON PILATUSimages FOLDER" | tee -a $LOG
counter=1
for file in $DIR/tests/testImages/PILATUSimages/*.tif
do
	echo "Test number $counter on $file..."
	musclex qf -h -i $file -s "$DIR/tests/testImages/PILATUSimages/qfsettings.json" -d > /dev/null
	let counter=counter+1
done

echo "
Done."

