(defsystem "crdt"
  :depends-on ("alexandria"
               "yason")
  :components ((:file "woot")))

(defsystem "crdt/test"
  :depends-on ("rove" "crdt")
  :components ((:file "woot-test")))
