#-eql5
(error "Please use the EQL5 executable")

(require :cmp)

(unless (member "norepl" (ext:command-args) :test #'string=)
  (push :app-repl *features*))

(load "lisp/dependencies")

(push "lisp/" asdf:*central-registry*)

(asdf:make-build "app"
                 :monolithic t
                 :type :static-library
                 :prologue-code '(progn (require :sb-bsd-sockets)
                                        (require :asdf))
                 :move-here "./"
                 :init-name "init_app")

(let ((lib-name "libapp.a"))
  (when (probe-file lib-name)
    (delete-file lib-name))
  (rename-file (x:cc "app--all-systems" ".a") lib-name))

(eql:qquit)
