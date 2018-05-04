# Updating Docs for Read-the-Docs

All the *.rst* and *.md* files (except *README.md*) under directory
`/docs` are related to the pages on http://musclex.readthedocs.io/.

Currently, *.rst* files are mainly used for the purpose of maintaining
directory structure. Both *.rst* and *.md* files are supported for
writing documents.

## Contents
* [How to make changes](#how-to-make-changes)
* [Details on editing Markdown](#details-on-editing-markdown)
* [Version control](#version-control)
* [Local test](#local-test)

## How to make changes
Since a webhook is set to connect to Read-the-Docs, every **commit** on
this Github repo will triger an updating action of pages on Read-the-Docs.

> Sometimes, the build process may fail. Records can be found at
  https://readthedocs.org/projects/musclex/builds/. Restart the build
  process or [test locally](#local-test) to see what went wrong.

### Modify an existing doc
Modify online by clicking the *Edit* button <svg width="14" height="16">
  <path fill-rule="evenodd" d="M0 12v3h3l8-8-3-3-8 8zm3 2H1v-2h1v1h1v1zm10.3-9.3L12 6 9 3l1.3-1.3a.996.996 0 0 1 1.41 0l1.59 1.59c.39.39.39 1.02 0 1.41z"></path>
</svg>
on the top-right side of the file viewer and commit changes.

### Create or delete an existing doc
Creating or deleting docs will change the directory structure. The
structure are maintained by the files named `index.rst` in all folders
containing docs. The main attribute is *toctree*. See an example from
`/docs/Installation/index.rst`:
```rst
.. toctree::
   :maxdepth: 2

   installer
   pip
   Docker
```
Thus, two steps are needed to create or delete an existing doc:
1. Create or delete the doc.
2. Modify the *toctree* in the corresponding `index.rst`, which is under
   the directory where the former doc is.

## Details on editing Markdown
### Relative links
All the docs are rendered to html files on Read-the-Docs, so the path of
a doc in a relative link should end with ".html" instead of ".md". See
an example from `AppSuite/Equator/Equator-(eq).md`:
```markdown
### More Details
* [The Equatorial Diffraction Pattern from Striated Muscle](The-Equatorial-Diffraction-Pattern-from-Striated-Muscle.html)
* [How it works](Equator--How-it-works.html)
* [How to use](Equator--How-to-use.html)
* [Results file](Equator--Summary.html)
```
For the path of image, both absolute path and relative path are accepted.
For example, the reference of the image in
`ReleaseNotes/Version-1.9.1.md` can be written as either
```markdown
![-](/images/CP/bandwidth.png)
```
(`/docs` is regarded as the root directory) or
```markdown
![-](../images/CP/bandwidth.png)
```
Using relative paths makes it possible to display images on both Github
and Read-the-Docs.

### Additional features
Additional markdown features provided by AutoStructify Component are
enabled in `conf.py`. (See [AutoStructify Component][4] for details.)

**Auto Doc Ref** is disabled since it does not work well now.

**Embed reStructuredText** is a useful feature. See an example of note
box from `Installation/pip.md`:
````markdown
```eval_rst
.. note:: Omit keyword *sudo* in the commands in following parts when working on Windows.
```
````
and the effect at http://musclex.readthedocs.io/en/latest/Installation/pip.html#python-3-6-on-windows.

## Version control
Edit the two global variable in `conf.py` whenever updating version
of the docs is needed.
```python
# The short X.Y version
version = ''
# The full version, including alpha/beta/rc tags
release = ''
```

## Local test
You are not able to see the effect of your changes immediately on
Read-the-Docs if you edit online via Github.

In the local repo of musclex, build the final html files:
```
cd docs
make html
```
The built html files can be seen in directory `docs/_build/html`.
(See more details [here][5].)

## Useful Guides
- [A beginner’s guide to writing documentation][1]
- [Restructured Text (reST) and Sphinx CheatSheet][2]
- [Markdown Cheatsheet][3]

[1]:http://docs.writethedocs.org/guide/writing/beginners-guide-to-docs/
[2]:https://thomas-cokelaer.info/tutorials/sphinx/rest_syntax.html
[3]:https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
[4]:https://recommonmark.readthedocs.io/en/latest/auto_structify.html
[5]:https://docs.readthedocs.io/en/latest/getting_started.html#write-your-docs