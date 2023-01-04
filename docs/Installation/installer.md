# Installation by Installer

For users who do not want to play with Python or Docker environment, we provide the pre-built installer for stand-alone MuscleX program.

[![Download MuscleX](https://a.fsdn.com/con/app/sf-download-button)](https://sourceforge.net/projects/musclex/files/)

## Windows

First of all, make sure you don't have a previous version of MuscleX installed on your computer, as it might create a conflict.

Download the [Windows Installer](https://sourceforge.net/projects/musclex/files/) built for Windows Pro 11(64-bit).

1. Find the shortcut of MuscleX on the Desktop or in the Start Menu, and click it to run
2. Test the program using test images installed to your `Documents` folder in the `MuscleX` sub-folder

## Mac OS X
There are 2 ways to install the Mac OS program for most versions.

```eval_rst
.. note:: For now, the packages are not signed using Apple's requirements, so you might experience some difficulties launching the app. We are working on this problem. For now, you can follow the procedure for a Dmg image.
```

### Pkg installer
Download the [pkg file](https://sourceforge.net/projects/musclex/files/) built for MacOS (MacOS >=10.15)

1. Run the pkg file, install the application.

### Dmg image
Download the [dmg file](https://sourceforge.net/projects/musclex/files/) built for MacOS (MacOS >=10.15)

1. Open the .dmg file and run the program inside.

```eval_rst
.. note:: IMPORTANT - If you see an error message (damaged disk or damaged program) follow these steps:

1. Open the .dmg file and copy the program on your computer (for example on the Desktop).
2. With a terminal opened at the root of the program (for example `cd Desktop`), run the following command: `xattr -cr musclex.app`
3. You can now eject the dmg file and delete it.
```

## Linux

### AppImage
Download the [AppImage file](https://sourceforge.net/projects/musclex/files/) built for Linux distributions.

1. Open a terminal and change to the directory where the AppImage file is located
2. Execute in the terminal `chmod u+x musclex-1.15.7-x86_64.AppImage`
3. Execute in the terminal `./musclex-1.15.7-x86_64.AppImage` to run the application

### Deb package
Download the [deb file](https://sourceforge.net/projects/musclex/files/) built for Linux distributions.

1. Open a terminal and run `sudo dpkg --install musclex-1.20_amd64(linux).deb`

### Troubleshooting

If the application crashes with the following error: `Gtk:ERROR:gtkiconhelper.c:494:ensure_surface_for_gicon: assertion failed (error == NULL): Failed to load /org/gtk/libgtk/icons/16x16/status/image-missing.png.`, it is probably due to environment variables that are set on your computer and are conflicting with the program. Two solutions:

* Creating a new clean user and installing Muscle X on this new user (the environment variables should be cleaner).

OR

* Opening a terminal and typing `printenv` to display all the environment variables. Then you need to unset the conflicting ones (save the previous values in a separate document in case you need to set them back, and unset them one by one). Here is a non-exhaustive list of the variables that might be conflicting:
```
unset QT_ACCESSIBILITY
unset GNOME_DESKTOP_SESSION_ID
unset XDG_CONFIG_DIR
unset XDG_CONFIG_DIRS
unset XDG_MENU_PREFIX
unset GNOME_SHELL_SESSION_MODE
unset XMODIFIERS
unset XAUTHORITY
unset WINDOWPATH
unset XDG_CURRENT_DESKTOP
unset GNOME_TERMINAL_SCREEN
unset GNOME_TERMINAL_SERVICE
unset QT_IM_MODULE
unset GDMSESSION
unset XDG_SESSION_DESKTOP
```

