# Building a DMG Image for MuscleX
The macOS spec builds the `.app` bundle directly with PyInstaller. Do not copy
the onedir output into `Contents/MacOS` manually; PyInstaller must place Python
and other frameworks in the bundle's `Contents/Frameworks` directory.

See [Building Fancy DMG Images on Mac OS X][1] for reference.

## Quick Method: Command Line Approach (Recommended)

This is the fastest way to create a DMG from the command line:

### 1. Build with PyInstaller
```bash
cd /path/to/musclex
pyinstaller --clean -y musclex_mac.spec
```

### 2. Validate the App Bundle
The app bundle is created by the spec and contains this layout:
```
musclex.app/
  Contents/
    MacOS/
      musclex-main
    Frameworks/
      Python
      Python.framework/
      ...
    Resources/
      AppIcon.icns
    Info.plist
```

Check that the bundle's executable and Python framework are present:
```bash
/usr/libexec/PlistBuddy -c 'Print :CFBundleExecutable' dist/musclex.app/Contents/Info.plist
test -e dist/musclex.app/Contents/Frameworks/Python
dist/musclex.app/Contents/MacOS/musclex-main eq -h
```

Set the version and sign the modified bundle:
```bash
VERSION=$(python -c "from musclex import __version__; print(__version__)")
PLIST=dist/musclex.app/Contents/Info.plist
/usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString ${VERSION}" "$PLIST"
codesign --force --deep --sign - dist/musclex.app
codesign --verify --deep --strict dist/musclex.app
```

### 3. Create the DMG
```bash
# Remove macOS quarantine attributes
xattr -cr dist/musclex.app

# Create DMG
mkdir -p dist/dmg_temp
cp -r dist/musclex.app dist/dmg_temp/
ln -s /Applications dist/dmg_temp/Applications
hdiutil create -volname "MuscleX 1.26.0" -srcfolder dist/dmg_temp -ov -format UDZO dist/musclex-1.26.0-macOS.dmg
rm -rf dist/dmg_temp

echo "✅ DMG created at: dist/musclex-1.26.0-macOS.dmg"
```

### 4. Test the App
```bash
# Test by opening directly
open dist/musclex.app

# Or test from terminal to see errors
dist/musclex.app/Contents/MacOS/musclex-main
```

### 5. Handle macOS Gatekeeper (if needed)
If you get "unidentified developer" warnings:
```bash
xattr -cr /Applications/MuscleX.app
```

Or right-click → hold Option key → Open → confirm.

### All-in-One Script

For convenience, here's a complete script that does everything:

```bash
#!/bin/bash
# Build MuscleX DMG for macOS
# Usage: ./build_mac_dmg.sh [version]

VERSION=${1:-1.26.0}
echo "Building MuscleX ${VERSION} for macOS..."

# 1. Build with PyInstaller
echo "Step 1: Building with PyInstaller..."
pyinstaller --clean -y musclex_mac.spec || exit 1

# 2. Set version and verify App Bundle
echo "Step 2: Verifying App Bundle..."
/usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString ${VERSION}" dist/musclex.app/Contents/Info.plist || exit 1
test -e dist/musclex.app/Contents/Frameworks/Python || exit 1
dist/musclex.app/Contents/MacOS/musclex-main eq -h || exit 1

# 3. Remove quarantine and create DMG
echo "Step 3: Creating DMG..."
xattr -cr dist/musclex.app
/usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString ${VERSION}" dist/musclex.app/Contents/Info.plist
codesign --force --deep --sign - dist/musclex.app
codesign --verify --deep --strict dist/musclex.app
mkdir -p dist/dmg_temp
cp -r dist/musclex.app dist/dmg_temp/
ln -s /Applications dist/dmg_temp/Applications
hdiutil create -volname "MuscleX ${VERSION}" -srcfolder dist/dmg_temp -ov -format UDZO "dist/musclex-${VERSION}-macOS.dmg"
rm -rf dist/dmg_temp

echo "✅ DMG created successfully: dist/musclex-${VERSION}-macOS.dmg"
echo "Test with: open dist/musclex.app"
```

---

## Alternative Method: Manual Disk Utility Approach

If you prefer using the GUI:

### Create a template image
The tool needed is *Disk Utility* (you can find it in the
`Applications/Utilities` folder on your Mac OS X).

Create a new blank image:
- **Name**: Muscle X
- **Size**: more than enough to store the whole App Buddle

Use default settings for other attributes like:
- **Encryption**: None
- **Format**: r/w

### Prepare the image
1. Open the newly created image.

2. Copy the App Bundle to the image  
  Assume that you are working in the root directory of musclex, and
  the newly created image is named "Muscle X":
```
cp -r dist/musclex.app /Volumes/Muscle\ X/
```

3. Create a soft link to **Applications** folder in the image
```
ln -s /Applications /Volumes/Muscle\ X/
```

4. Eject the image

### Build the final DMG
Use *Disk Utility* again.

1. Select *Convert…* from the *Images* menu.
2. Open the prepared image.
3. Enter the final name, like "musclex-1.21.1(macOS)".
4. Select *Compressed* as Image Format.
5. *Save* it.



---

## Building pkg file for MuscleX (Optional)

Once the Mac OS App Bundle has been built by PyInstaller, you can use **Packages** to build the pkg file.

See [Using Packages to create an installer for distributing macOS apps outside of the Mac App Store][2] for reference. 


[1]:https://el-tramo.be/blog/fancy-dmg/  
[2]:https://www.appcoda.com/packages-macos-apps-distribution/ 
