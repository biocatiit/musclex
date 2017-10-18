FROM ubuntu:16.04
# Install packages.
RUN apt-get update
RUN apt-get install -y python python-dev python-pip
RUN apt-get install -y libjpeg-dev libopencv-dev python-opencv
RUN apt-get install -y python-qt4
RUN apt-get install -y cython
RUN apt-get install -y pyfai
RUN apt-get install -y gfortran
RUN pip install --upgrade pip
RUN pip install --upgrade musclex==1.3a
# RUN python -m unittest discover -s /tests -p '*_test.py'
