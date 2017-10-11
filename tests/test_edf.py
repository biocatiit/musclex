import fabio
from PyQt4 import QtGui
import sys
import matplotlib.pyplot as plt
from tifffile import imsave

img_path = "/Users/preawwy/RA/Data/edf_files/B02_frelon_00040_raw.edf"
file = fabio.open(img_path)
print file.header
data = file.data

fig = plt.figure()
ax = fig.add_subplot(111)
ax.imshow(data)
fig.show()

imsave("B02_frelon_00040_raw.tif", data)
# app = QtGui.QApplication(sys.argv)
# sys.exit(app.exec_())