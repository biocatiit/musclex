from PyInstaller.utils.hooks import collect_dynamic_libs

hiddenimports = ['sklearn.metrics._pairwise_distances_reduction._datasets_pair','sklearn.metrics._pairwise_distances_reduction._middle_term_computer']
binaries = collect_dynamic_libs('sklearn')