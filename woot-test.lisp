(uiop:define-package #:crdt/test
  (:use #:cl
        #:rove
        #:crdt/woot))
(in-package #:crdt/test)

(deftest woot-test
  (let ((doc (make-document 1)))
    (generate-insert doc 0 "a")
    (generate-insert doc 1 "b")
    (generate-insert doc 2 "c")
    (ok (equal "abc" (get-string doc))))

  (let ((doc (make-document 1)))
    (generate-insert doc 0 "a")
    (let ((char (generate-insert doc 1 "b")))
      (generate-insert doc 2 "c")
      (delete-char doc char))
    (ok (equal "ac" (get-string doc))))

  (let ((doc1 (make-document 1))
        (doc2 (make-document 2)))
    (let ((char1 (generate-insert doc1 0 "a"))
          (char2 (generate-insert doc2 0 "b")))
      (insert-char doc2 char1)
      (insert-char doc1 char2)
      (ok (equal "ab" (get-string doc2)))
      (ok (equal "ab" (get-string doc1)))))

  (let ((doc1 (make-document 1))
        (doc2 (make-document 2)))
    (let ((char1 (generate-insert doc1 0 "a"))
          (char2 (generate-insert doc2 0 "b"))
          (char3 (generate-insert doc1 0 "c")))
      (insert-char doc2 char1)
      (insert-char doc1 char2)
      (insert-char doc2 char3)
      (list (get-string doc1)
            (get-string doc2)))))
