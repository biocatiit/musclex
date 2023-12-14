# Guide to Freezing NVIDIA Drivers in Linux

This guide provides a step-by-step process for developers on how to freeze NVIDIA drivers in a Linux environment and prevent system updates from altering these drivers. By following these instructions, you can maintain a stable version of the NVIDIA drivers that are compatible with your specific requirements.

## Prerequisites

- Linux-based operating system
- Root or sudo privileges
- NVIDIA drivers already installed

## Step 1: Verify Current NVIDIA Driver Version

Before proceeding, ensure you know the version of the currently installed NVIDIA drivers.

```bash
nvidia-smi
```

## Step 2: Hold the NVIDIA Package

To prevent the NVIDIA driver from being updated, you need to mark it as held in your package management system. The command varies depending on the distribution you are using.

### For Debian/Ubuntu-based systems:

```bash
sudo apt-mark hold nvidia-driver-[version]
```

Replace `[version]` with the actual version number of your NVIDIA driver.

### For Red Hat/Fedora-based systems:

```bash
sudo dnf versionlock add nvidia-driver-[version]
```

Again, replace `[version]` with your driver version number.

### For Arch Linux:

Arch Linux uses a rolling release model, which might require a different approach like downgrading and holding the package. You can use the Arch Wiki for specific instructions.

## Step 3: Update Your System

Now that the NVIDIA driver is held, you can safely update your system without affecting the NVIDIA driver.

### Debian/Ubuntu:

```bash
sudo apt update && sudo apt upgrade
```

### Red Hat/Fedora:

```bash
sudo dnf upgrade
```

### Arch Linux:

Refer to the Arch Wiki for safe system update practices.

## Step 4: Verify the Hold Status

After the system update, verify that the NVIDIA driver version has not changed.

```bash
nvidia-smi
```

## Step 5: Unholding the Driver (Optional)

If you need to update the NVIDIA driver in the future, you can unhold it using the following commands.

### Debian/Ubuntu:

```bash
sudo apt-mark unhold nvidia-driver-[version]
```

### Red Hat/Fedora:

```bash
sudo dnf versionlock delete nvidia-driver-[version]
```

## Conclusion

By following these steps, you can effectively freeze your NVIDIA driver version in a Linux environment, ensuring that system updates do not disrupt your development environment or specific driver requirements. Always remember to consult your specific Linux distribution's documentation for any nuances in package management.