#!/bin/bash
git stash
git pull
rm /usr/local/bin/bio_muscle
rm /usr/local/bin/quadrant_folding
rm /usr/local/bin/diffraction_centroids
rm /usr/local/bin/ddf_processor

ln -s /usr/local/bin/biocat/bio_muscle.py /usr/local/bin/bio_muscle
ln -s /usr/local/bin/biocat/quadrant_folding.py /usr/local/bin/quadrant_folding
ln -s /usr/local/bin/biocat/ddf_processor.py /usr/local/bin/ddf_processor
ln -s /usr/local/bin/biocat/diffraction_centroids.py /usr/local/bin/diffraction_centroids

chmod +x /usr/local/bin/bio_muscle
chmod +x /usr/local/bin/quadrant_folding
chmod +x /usr/local/bin/diffraction_centroids
chmod +x /usr/local/bin/ddf_processor

rm -rf /usr/local/bin/biocat/Quadrant_Folding/code/build
rm /usr/local/bin/biocat/Quadrant_Folding/code/QF_utilities.so
rm /usr/local/bin/biocat/Quadrant_Folding/code/QF_utilities.c

python /usr/local/bin/biocat/biocat_modules/setup2.py build_ext --inplace