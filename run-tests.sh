## run in subdir because test files there there
EMACS=${EMACS-emacs}
$EMACS -batch -l ./test/dash.el  -l m-buffer.el -l ./test/m-buffer-test.el \
    -f ert-run-tests-batch-and-exit
