# from bio_muscle import BMStartWindow
# from quadrant_folding import QuadrantFoldingGUI
# from diffraction_centroids import DiffractionCentroidStartWindow
# from circular_projection_v2 import CircularProjectionGUI
# from ddf_processor import DDFWindow
import sys
# from PyQt4 import QtGui

if __name__ == "__main__":
    if len(sys.argv) > 2:
        program = sys.argv[1]
        print program
    # app = QtGui.QApplication(sys.argv)
    # myapp = BMStartWindow()
    # sys.exit(app.exec_())