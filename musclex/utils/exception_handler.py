"""
Copyright 1999 Illinois Institute of Technology

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL ILLINOIS INSTITUTE OF TECHNOLOGY BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Illinois Institute
of Technology shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from Illinois Institute of Technology.
"""

import traceback
import subprocess
import os.path

def mac_except_hook(*exc_info):
    """
    Used to write the exception hook for mac and execute them with osascript
    """
    fname = os.path.expanduser('~/Documents/musclex.error.log')
    with open(fname, 'a') as f:
        f.write(''.join(traceback.format_exception(*exc_info)))
        f.close()
    applescript = "'\
tell app \"System Events\" to \
display dialog \"Error details are written to the file: {}.\" \
with title \"Error message\" \
with icon caution \
buttons {{\"OK\"}}'".format(fname)
    subprocess.Popen('osascript -e ' + applescript, shell=True)

handlers = {
    'darwin': mac_except_hook
}
