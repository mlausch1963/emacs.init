import sys, os


# do nothing if not in virtual env,
# otherwise execute pylint  in virtual envuronment, using activate_this.py
if 'VIRTUAL_ENV' in os.environ:
    ve_dir = os.environ['VIRTUAL_ENV']
    ve_dir in sys.path or sys.path.insert(0, ve_dir)
    activate_this = os.path.join(os.path.join(ve_dir, 'bin'), 'activate_this.py')

    # Fix for windows
    if not os.path.exists(activate_this):
        activate_this = os.path.join(os.path.join(ve_dir, 'Scripts'), 'activate_this.py')

    exec(open(activate_this).read(), dict(__file__=activate_this))
