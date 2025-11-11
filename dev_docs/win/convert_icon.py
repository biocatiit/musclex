#!/usr/bin/env python3
"""
Convert AppIcon.icns to AppIcon.ico for Windows
Run this from dev_docs/win/ directory
"""

from PIL import Image
import os

def convert_icns_to_ico():
    """Convert the macOS .icns file to Windows .ico format"""
    
    input_file = '../linux/AppIcon.icns'
    output_file = 'AppIcon.ico'
    
    print(f"Converting {input_file} to {output_file}...")
    
    try:
        # Open the .icns file from linux folder
        img = Image.open(input_file)
        
        # Get the size of the loaded image
        print(f"Original image size: {img.size}")
        
        # Save as .ico with multiple sizes for Windows
        # Windows ICO files typically contain multiple resolutions
        sizes = [(16, 16), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)]
        
        img.save(output_file, 
                 format='ICO', 
                 sizes=sizes)
        
        print(f"✓ Successfully converted to {output_file}")
        print(f"  Icon includes sizes: {', '.join([f'{s[0]}x{s[1]}' for s in sizes])}")
        
        # Verify the output file was created
        if os.path.exists(output_file):
            file_size = os.path.getsize(output_file)
            print(f"  Output file size: {file_size:,} bytes")
        
    except Exception as e:
        print(f"✗ Error converting icon: {e}")
        print("\nTroubleshooting:")
        print("1. Make sure Pillow is installed: pip install Pillow")
        print("2. Check that ../linux/AppIcon.icns exists")
        return False
    
    return True

if __name__ == '__main__':
    print("=" * 60)
    print("MuscleX Icon Converter - .icns to .ico for Windows")
    print("=" * 60)
    
    # Check if input file exists
    if not os.path.exists('../linux/AppIcon.icns'):
        print("✗ Error: ../linux/AppIcon.icns not found")
        print("  Please run this script from dev_docs/win/")
        exit(1)
    
    # Perform conversion
    success = convert_icns_to_ico()
    
    if success:
        print("\n" + "=" * 60)
        print("Conversion complete!")
        print("You can now use AppIcon.ico for Windows builds")
        print("=" * 60)
    else:
        exit(1)

