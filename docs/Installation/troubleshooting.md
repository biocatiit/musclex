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



### 4K Display Issues

If buttons or text are too small on high-resolution screens, try:

```bash
export QT_AUTO_SCREEN_SCALE_FACTOR=1
```



## Need Help?

If you’ve reviewed the relevant install method pages and this guide but are still stuck:

- Open an issue on [GitHub](https://github.com/biocatiit/musclex/issues)
- Refer to the [MuscleX documentation](https://musclex.readthedocs.io/)
