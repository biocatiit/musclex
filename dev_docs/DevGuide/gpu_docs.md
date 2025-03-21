# Check GPU:

First check the type of GPU mounted on your computer with
```
	hwinfo --gfxcard --short
```

Then install the corresponding drivers if it is not already the case. 

## NVIDIA

For NVIDIA card, proceed as following to install the drivers.

Look for the drivers available:
```
	apt search nvidia-driver
```

Select a version of the driver, the latest is better (here it was 515):
```
	sudo apt update
	sudo apt upgrade
	sudo apt install nvidia-driver-515 nvidia-dkms-515
```

Reboot the computer:
```
	sudo reboot
```

Test if the driver works:
```
	nvidia-smi
```

Benchmark the GPU:
```
	glxgears
```
(if not installed, run the command : `sudo apt-get install mesa-utils`)

To access the GPU settings:
```
	nvidia-settings
```

# Cuda

## Installation

Follow this link to install Cuda:

[https://developer.nvidia.com/cuda-downloads](https://developer.nvidia.com/cuda-downloads)

## Testing

Follow this link to test Cuda:

[https://xcat-docs.readthedocs.io/en/stable/advanced/gpu/nvidia/verify_cuda_install.html](https://xcat-docs.readthedocs.io/en/stable/advanced/gpu/nvidia/verify_cuda_install.html)

# Tensorflow

## Installation and testing

Follow this link to install and test tensorflow:

[https://www.tensorflow.org/install/pip](https://www.tensorflow.org/install/pip)
