# Building DMG Image for MuscleX
Make sure you have finished building the Mac OS X App Bundle of MuscleX
with PyInstaller.

See [Building Fancy DMG Images on Mac OS X][1] for reference.

## Create a template image
The tool needed is *Disk Utility* (you can find it in the
`Applications/Utilities` folder on your Mac OS X).

Create a new blank image:
- **Name**: Muscle X
- **Size**: more than enough to store the whole App Buddle

Use default settings for other attributes like:
- **Encryption**: None
- **Format**: r/w

## Prepare the image
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

## Build the final DMG
Use *Disk Utility* again.

1. Select *Convert…* from the *Images* menu.
2. Open the prepared image.
3. Enter the final name, like "musclex-1.21.1(macOS)".
4. Select *Compressed* as Image Format.
5. *Save* it.



# Building pkg file for MuscleX  
Once the MAC OS APP bundle was built by pyinstaller  
Use packages to build the pkg file  

See [Using Packages to create an installer for distributing macOS apps outside of the Mac App Store][2] for reference. 


[1]:https://el-tramo.be/blog/fancy-dmg/  
[2]:https://www.appcoda.com/packages-macos-apps-distribution/ 



