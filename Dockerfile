FROM ubuntu:16.04
# Install packages.
RUN apt-get update
RUN apt-get install -y python python-dev python-pip
RUN apt-get install -y libjpeg-dev libopencv-dev python-opencv
RUN apt-get install -y python-qt4
RUN apt-get install -y cython
RUN apt-get install -y pyfai
RUN apt-get install -y gfortran
#RUN pip install --upgrade musclex

RUN pip install --upgrade pip

ADD musclex /musclex/musclex
#ADD tests /
ADD LICENSE.txt /musclex/LICENSE.txt
ADD MANIFEST /musclex/MANIFEST
ADD README.md /musclex/README.md
ADD setup.cfg /musclex/setup.cfg
ADD setup.py /musclex/setup.py
ENV PYTHONPATH /musclex/:$PYTHONPATH
WORKDIR /musclex/
RUN python /musclex/setup.py install

ADD tests /musclex/tests
RUN python -m unittest discover -s /musclex/tests -p '*_test.py'
