.. -*- mode: rst -*-

Execute pylint in pylsp in virtenv
----------------------------------

pylsp calls pylint exec witht he help of elpylint module. This starts the pylint
executable, but witout setting it up correctly.
Using the pyvenv package of emacs to manage yourt virtualenvironments
and  the init_hook in pylintrc this can be corrected.

1. make sure there is an activate_this.py file in the virtualenv. It's missing
   if the virtualenv was created using ``pyhton -m venv <name>`` instead of
   ``virtualenv`` If not a copy is in this directory. Copy it in the bin directory
   of the virtual environment

2. Create a pylinrc.py file somewhere. I put it in my ``~/bin`` folder.
   The file is a simple wrapper to execute  the ``activate_this.py``
   file. This is only necessary, because the .pylintrc file does not allow
   multiline content. And I have not found an easy way to implement this in one
   line (no '\\' allowed)
   Here's the content of the file.::

     import sys, os

     if 'VIRTUAL_ENV' not in os.environ:
         sys.exit(0)

     ve = os.environ['VIRTUAL_ENV']
     ve in sys.path or sys.path.insert(0, ve)
     activate_this = os.path.join(os.path.join(ve, 'bin'), 'activate_this.py')
     exec(open(activate_this).read(), dict(__file__=activate_this))


3. just anotgher line
   Create a pylintrc file. ::

     pylint --generate-rcfile > ~/.pylintrc

   should do
   the trick. Change the init-hook to
   ``init-hook="import imp, os; imp.load_source('import_hook',os.path.join(os.env.get('HOME'), 'bin', 'pylintrc.py'))``

To automatically switch to the correct virtual environment when working on a
python project I put the following into the ``.dir-locals.el`` file in the root
dir of the project. ::

  ;;; Directory Local Variables
  ;;; For more information see (info "(emacs) Directory Variables")

  ((python-mode
    (eval . (pyvenv-mode t))
    (pyvenv-activate . "path to the virtual environment/home/mla/python-envs/prom-mandatory-rule-check")))
