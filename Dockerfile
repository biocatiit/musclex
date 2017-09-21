FROM ubuntu:16.04
# Install packages.
RUN apt-get update
RUN apt-get install -y python python-dev python-pip
RUN apt-get install -y libjpeg-dev libopencv-dev python-opencv
RUN apt-get install -y python-qt4
RUN apt-get install -y pyfai
RUN pip install --upgrade pip
RUN pip install --upgrade scikit-image
RUN pip install --upgrade tifffile
RUN pip install --upgrade numpy
RUN pip install --upgrade pandas
RUN pip install --upgrade scikit-learn
RUN pip install --upgrade lmfit
RUN pip install --upgrade ConfigParser
RUN pip install --upgrade pillow
RUN pip install --upgrade fabio
RUN pip install --upgrade cython
RUN pip install --upgrade peakutils
RUN pip install --upgrade h5py
RUN pip install --upgrade scipy
RUN pip install --upgrade matplotlib
ADD bio_muscle.py /
ADD stylesheet.txt /
ADD quadrant_folding.py /
ADD circular_projection_v2.py /
ADD diffraction_centroids.py /
ADD ddf_processor.py /
ADD bio_utils /bio_utils
ADD ui /ui
ADD csv_manager /csv_manager
ADD biocat_modules /biocat_modules
ADD CalibrationSettings /CalibrationSettings
ADD Data /Sample_Data
ADD tests /tests
RUN python /biocat_modules/setup.py build_ext --inplace
RUN python -m unittest discover -s /tests -p '*_test.py'
