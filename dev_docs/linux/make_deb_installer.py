# Run this script after doing the pyinstaller build

import os
import shutil
import subprocess
from musclex import __version__

os.sys.path.append(os.path.abspath(os.path.join('..', '..')))

deb_path = os.path.join('.', 'musclex-{}_amd64(linux)'.format(__version__),
    'DEBIAN')
exc_path = os.path.join('.', 'musclex-{}_amd64(linux)'.format(__version__),
    'usr', 'bin')
app_path = os.path.join('.', 'musclex-{}_amd64(linux)'.format(__version__),
    'usr', 'share', 'applications')
png_path = os.path.join('.', 'musclex-{}_amd64(linux)'.format(__version__),
    'usr', 'share', 'icons', 'hicolor', '48x48', 'apps')
lib_path = os.path.join('.', 'musclex-{}_amd64(linux)'.format(__version__),
    'usr', 'share', 'musclex')

os.makedirs(deb_path, exist_ok=True)
os.makedirs(exc_path, exist_ok=True)
os.makedirs(app_path, exist_ok=True)
os.makedirs(png_path, exist_ok=True)

shutil.copy('control', deb_path)
shutil.copy(os.path.join('..', '..', 'dist', 'musclex', 'musclex-launcher'), exc_path)
shutil.copytree(os.path.join('..', '..', 'dist', 'musclex'), lib_path)
shutil.copy('musclex.desktop', app_path)
shutil.copy(os.path.join('..', '..', 'dist', 'musclex.app', 'Contents',
            'Resources', 'AppIcon.icns'), os.path.join(png_path, 'AppIcon.icns'))

with open(os.path.join(deb_path, 'control'), 'r') as f:
    control_lines = f.readlines()

for i in range(len(control_lines)):
    if control_lines[i].startswith('Version'):
        control_lines[i] = 'Version: {}\n'.format(__version__)

with open(os.path.join(deb_path, 'control'), 'w') as f:
    f.writelines(control_lines)

with open(os.path.join(app_path, 'musclex.desktop'), 'r') as f:
    control_lines = f.readlines()

for i in range(len(control_lines)):
    if control_lines[i].startswith('Version'):
        control_lines[i] = 'Version={}\n'.format(__version__)

with open(os.path.join(app_path, 'musclex.desktop'), 'w') as f:
    f.writelines(control_lines)

proc = subprocess.Popen("fakeroot dpkg-deb --build musclex-{}_amd64\(linux\)".format(__version__),
    shell=True)
proc.communicate()

print('Checking .deb installer with lintian')
proc = subprocess.Popen("lintian musclex-{}_amd64\(linux\).deb".format(__version__), shell=True)
proc.communicate()
