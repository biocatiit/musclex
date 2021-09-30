import traceback, subprocess, os.path

def mac_except_hook(*exc_info):
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
    # print(applescript)
    subprocess.Popen('osascript -e ' + applescript, shell=True)

handlers = {
    'darwin': mac_except_hook
}