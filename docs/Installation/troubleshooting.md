# Troubleshooting

This page covers cross-platform and GUI-related issues that may arise when using MuscleX, regardless of installation method.



### GTK Icon Error (Linux)

If you see this error:

```
Gtk:ERROR:gtkiconhelper.c:494:ensure_surface_for_gicon: assertion failed (error == NULL)
```

**Solution:**

- Create a clean user account and try running MuscleX there
- Or, try unsetting potentially conflicting environment variables:

```bash
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



### Qt Platform Plugin "xcb" Error (Linux)

If you see an error similar to:

```
qt.qpa.plugin: Could not load the Qt platform plugin "xcb" in "" even though it was found.
This application failed to start because no Qt platform plugin could be initialized.
Reinstalling the application may fix this problem.
```

(Often reported with Qt 6.x, e.g. `Qt version: 6.7.2`, when launching tools such as `musclex xv`.)

This typically means the system is missing native XCB / X11 libraries that the Qt `xcb` platform plugin depends on, or that an `opencv-python` install is shipping a conflicting Qt runtime.

**Solution (Debian/Ubuntu):** install the required system libraries:

```bash
sudo apt update
sudo apt install libxcb-cursor0 libxkbcommon-x11-0 libxcb-xinerama0
sudo apt install libxcb-render-util0 libxcb-image0 libxcb-keysyms1 libxcb-icccm4
sudo apt install x11-apps qt6-base-plugins qt6-base-dev
sudo apt-get install python3-opencv
```

**Diagnosing further:** rerun the failing command with Qt plugin debug output enabled to see exactly which library is missing:

```bash
QT_DEBUG_PLUGINS=1 musclex xv
```

If the issue persists in a `pip` environment, also see the *Qt platform plugin error* entry in [pip.md](pip.md) — replacing `opencv-python` with `opencv-python-headless` avoids a bundled Qt that can clash with the system one.



### 4K Display Issues

If buttons or text are too small on high-resolution screens, try:

```bash
export QT_AUTO_SCREEN_SCALE_FACTOR=1
```



## Need Help?

If you’ve reviewed the relevant install method pages and this guide but are still stuck:

- Open an issue on [GitHub](https://github.com/biocatiit/musclex/issues)
- Refer to the [MuscleX documentation](https://musclex.readthedocs.io/)
