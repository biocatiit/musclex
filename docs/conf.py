# Configuration file for the Sphinx documentation builder.

import os

# -- Project information -----------------------------------------------------

project = 'MuscleX'
copyright = '2025, BioCAT'
author = 'BioCAT'

version = '1.25'
release = '1.25.0'

# -- General configuration ---------------------------------------------------

extensions = [
    'sphinx.ext.mathjax',
    'myst_parser'
]

myst_enable_extensions = [
    "tables",             # Enables GitHub-style pipe tables
    "colon_fence",        # ::: fenced blocks (useful for admonitions)
    "deflist",            # Definition lists
    "fieldlist",          # Field lists (like function args)
    "html_admonition",    # HTML-style admonitions
    "html_image",         # <img> tags in markdown
    "replacements",       # Typographic replacements (e.g., -- to en dash)
    "smartquotes"         # Smart quotes
]

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

master_doc = 'index'
language = 'en'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'README.md']
pygments_style = 'sphinx'

# -- Options for HTML output -------------------------------------------------

html_theme = 'sphinx_rtd_theme'
html_theme_options = {
    'logo_only': False
}

# Disable static path warning since _static does not exist
html_static_path = []

# -- Options for HTMLHelp output ---------------------------------------------

htmlhelp_basename = 'musclexdoc'

# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # Customize if needed
}

latex_documents = [
    (master_doc, 'musclex.tex', 'MuscleX Documentation',
     'BioCAT', 'manual'),
]

# -- Options for manual page output ------------------------------------------

man_pages = [
    (master_doc, 'musclex', 'MuscleX Documentation',
     [author], 1)
]

# -- Options for Texinfo output ----------------------------------------------

texinfo_documents = [
    (master_doc, 'musclex', 'MuscleX Documentation',
     author, 'musclex', 'One line description of project.',
     'Miscellaneous'),
]

