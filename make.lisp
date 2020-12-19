#-eql5
(error "Please use the EQL5 executable")

(require :cmp)

;; Uncomment this to enable Slime / Sly.
;(push :app-debug *features*)

(load "lisp/dependencies")

(push "lisp/" asdf:*central-registry*)

(asdf:make-build "app"
                 :monolithic t
                 :type :static-library
                 :prologue-code '(require :ecl-quicklisp)
                 :move-here "./"
                 :init-name "init_lib_APP__ALL_SYSTEMS")

(let ((lib-name "libapp.a"))
  (when (probe-file lib-name)
    (delete-file lib-name))
  (rename-file (x:cc "app--all-systems" ".a") lib-name))

(eql:qquit)
