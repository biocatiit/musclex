import fabio
from tifffile import imsave
import numpy as np

fibrefix_bg = fabio.open('/Users/preawwy/RA/Data/DC_data/off_meridian/qf_results/bg/bg_fibrefix_r11-300_bin10_p5-25_s1.jpg').data
qf_bg= fabio.open('/Users/preawwy/RA/Data/DC_data/off_meridian/qf_results/bg/Prep24-1-1-Con.tif.bg.tif').data


ratios = np.array((1.*fibrefix_bg)/qf_bg)
ratios[ratios>300.] = 300.
ratios[ratios < 0] = 0.
ratios = ratios.astype('float32')
imsave('ratio.tif', ratios)