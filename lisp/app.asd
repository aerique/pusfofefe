(defsystem app
  :serial t
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :around-compile (lambda (thunk)
                    #+app-debug
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall thunk))
  :depends-on ("drakma" "jsown" "cloverlover")
  :components ((:file "qml")
               (:file "app")
               (:file "cloverlover")))
