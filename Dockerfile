FROM ubuntu:22.04
# Install packages.
ENV TZ=US
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update
RUN apt-get install -y python3 python3-dev python3-pip gfortran
RUN apt-get install '^libxcb.*-dev' libx11-xcb-dev libglu1-mesa-dev libxrender-dev libxi-dev libxkbcommon-dev libxkbcommon-x11-dev

RUN pip3 install --upgrade pip
RUN pip3 install --upgrade distro
RUN pip3 install --upgrade pyopencl
RUN pip3 install --upgrade cython
RUN pip3 install --upgrade numpy
RUN pip3 install --upgrade opencv-python
RUN pip3 install --upgrade pyfai
RUN pip3 install --upgrade PyQt5
RUN pip3 install --upgrade musclexflibs
RUN pip3 install --upgrade fisx
RUN pip3 install --upgrade future

RUN pip install --upgrade musclex
#RUN pip3 install git+https://github.com/biocatiit/musclex.git@v1.15.4


#ADD musclex /musclex/musclex
#
#ADD LICENSE.txt /musclex/LICENSE.txt
#ADD MANIFEST /musclex/MANIFEST
#ADD README.md /musclex/README.md
#ADD setup.cfg /musclex/setup.cfg
#ADD setup.py /musclex/setup.py
#ENV TMP_PATH $PYTHONPATH
#ENV PYTHONPATH /musclex/:$TMP_PATH
#WORKDIR /musclex/
#RUN python /musclex/setup.py install
#
