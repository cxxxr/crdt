(uiop:define-package #:crdt/woot
  (:use #:cl
        #:alexandria)
  (:export #:make-document
           #:char-id-site-id
           #:char-id-local-id
           #:copy-woot-char
           #:woot-char-id
           #:woot-char-visible
           #:woot-char-value
           #:woot-char-next
           #:woot-char-previous
           #:generate-insert
           #:generate-delete
           #:insert-char
           #:delete-char
           #:make-character-from-hash
           #:replace-with-woot-sequence
           #:get-string
           #:char-position))
(in-package #:crdt/woot)

(defvar *local-id* 0)

(defstruct char-id
  (site-id (error "site-id is required"))
  (local-id (incf *local-id*)))

(defun char-id-equal (id1 id2)
  (and (equal (char-id-site-id id1)
              (char-id-site-id id2))
       (= (char-id-local-id id1)
          (char-id-local-id id2))))

(defun char-id< (id1 id2)
  (and (string< (princ-to-string (char-id-site-id id1))
                (princ-to-string (char-id-site-id id2)))
       (or (equal (char-id-site-id id1)
                  (char-id-site-id id2))
           (< (char-id-local-id id1)
              (char-id-local-id id2)))))

(defclass woot-char ()
  ((id :initarg :id
       :type char-id
       :accessor woot-char-id)
   (visible :initarg :visible
            :type boolean
            :accessor woot-char-visible)
   (value :initarg :value
          :type (or character string)
          :accessor woot-char-value)
   (next :initarg :next
         :initform nil
         :type (or null char-id)
         :accessor woot-char-next)
   (previous :initarg :previous
             :initform nil
             :type (or null char-id)
             :accessor woot-char-previous)))

(defmethod print-object ((object woot-char) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'id)
      (format stream
              "~A:~A:~A"
              (char-id-site-id (woot-char-id object))
              (char-id-local-id (woot-char-id object))
              (woot-char-value object)))))

(defun copy-woot-char (woot-char)
  (make-instance 'woot-char
                 :id (woot-char-id woot-char)
                 :visible (woot-char-visible woot-char)
                 :value (woot-char-value woot-char)
                 :next (woot-char-next woot-char)
                 :previous (woot-char-previous woot-char)))

(defun woot-char-equal (char1 char2)
  (char-id-equal (woot-char-id char1)
                 (woot-char-id char2)))

(defclass document ()
  ((seq :initarg :sequence
        :accessor document-sequence)
   (start :initarg :start
          :accessor document-start)
   (end :initarg :end
        :accessor document-end)
   (site-id :initarg :site-id
            :accessor document-site-id)))

(defun make-document (site-id)
  (let ((start (make-instance 'woot-char
                              :id (make-char-id :site-id -1)
                              :value "start"
                              :visible nil))
        (end (make-instance 'woot-char
                            :id (make-char-id :site-id -1)
                            :value "end"
                            :visible nil)))
    (setf (woot-char-next start) (woot-char-id end)
          (woot-char-previous end) (woot-char-id start))
    (make-instance 'document
                   :sequence (list start end)
                   :start start
                   :end end
                   :site-id site-id)))

(defun nth-visible (sequence position)
  (assert (<= 0 position))
  (elt (remove-if-not #'woot-char-visible sequence) position))

(defun position-by-char-id (sequence char-id)
  (position char-id sequence :key #'woot-char-id :test #'char-id-equal))

(defun sub-sequence (sequence prev next)
  (let ((start-pos (position-by-char-id sequence (woot-char-id prev)))
        (end-pos (position-by-char-id sequence (woot-char-id next))))
    (subseq sequence (1+ start-pos) end-pos)))

(defun local-insert (sequence char position)
  (assert (< 0 position (length sequence)))
  (setf sequence (append (subseq sequence 0 position)
                         (list char)
                         (subseq sequence position)))
  (setf (woot-char-next (elt sequence (1- position))) (woot-char-id char)
        (woot-char-previous (elt sequence (1+ position))) (woot-char-id char))
  sequence)

(defun integrate-insert (sequence char prev next)
  (let ((sub-sequence (sub-sequence sequence prev next))
        (pos (position-by-char-id sequence (woot-char-id next))))
    (cond ((length= sub-sequence 0)
           (local-insert sequence char pos))
          ((length= sub-sequence 1)
           (local-insert sequence char (1- pos)))
          (t
           (let ((i 1))
             (loop :while (and (< i (1- (length sub-sequence)))
                               (char-id< (woot-char-id (elt sub-sequence i))
                                         (woot-char-id char)))
                   :do (incf i))
             (integrate-insert sequence
                               char
                               (elt sub-sequence (1- i))
                               (elt sub-sequence i)))))))

(defun insert-char-internal (sequence char)
  (let ((prev (find (woot-char-previous char) sequence :key #'woot-char-id :test #'char-id-equal))
        (next (find (woot-char-next char) sequence :key #'woot-char-id :test #'char-id-equal)))
    (assert prev)
    (assert next)
    (integrate-insert sequence char prev next)))

(defun insert-char (document char)
  (setf (document-sequence document)
        (insert-char-internal (document-sequence document) char))
  (values))

(defun generate-insert (document position value)
  (let* ((sequence (document-sequence document))
         (visible-sequence (append (list (elt sequence 0))
                                   (remove-if-not #'woot-char-visible sequence)
                                   (list (elt sequence (1- (length sequence))))))
         (prev (elt visible-sequence position))
         (next (elt visible-sequence (1+ position)))
         (char (make-instance 'woot-char
                              :id (make-char-id :site-id (document-site-id document))
                              :visible t
                              :value value
                              :next (woot-char-id next)
                              :previous (woot-char-id prev))))
    (insert-char document char)
    char))

(defun generate-delete (document position)
  (when-let ((char (nth-visible (document-sequence document) position)))
    (setf (woot-char-visible char) nil)
    char))

(defun delete-char (document char)
  (when-let* ((sequence (document-sequence document))
              (pos (position-by-char-id sequence (woot-char-id char)))
              (char (elt sequence pos)))
    (when (woot-char-visible char)
      (setf (woot-char-visible char) nil)
      t)))

(defmethod yason:encode ((object char-id) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "site-id" (char-id-site-id object))
      (yason:encode-object-element "local-id" (char-id-local-id object)))))

(defmethod yason:encode ((object woot-char) &optional stream)
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:with-object-element ("id")
        (yason:encode (woot-char-id object) stream))
      (yason:encode-object-element "visible" (woot-char-visible object))
      (yason:encode-object-element "value" (woot-char-value object))
      (yason:with-object-element ("next")
        (yason:encode (woot-char-next object) stream))
      (yason:with-object-element ("previous")
        (yason:encode (woot-char-previous object) stream)))))

(defun make-id-from-hash (hash)
  (if (null hash)
      nil
      (let ((site-id (gethash "site-id" hash))
            (local-id (gethash "local-id" hash)))
        (make-char-id :site-id site-id
                      :local-id local-id))))

(defun make-character-from-hash (hash)
  (let ((id (make-id-from-hash (gethash "id" hash)))
        (visible (gethash "visible" hash))
        (value (gethash "value" hash))
        (next (make-id-from-hash (gethash "next" hash)))
        (previous (make-id-from-hash (gethash "previous" hash))))
    (make-instance 'woot-char
                   :id id
                   :visible visible
                   :value value
                   :next next
                   :previous previous)))

(defun replace-with-woot-sequence (document woot-sequence)
  (setf (document-sequence document) woot-sequence))

(defun get-string (document)
  (with-output-to-string (out)
    (loop :for char :in (document-sequence document)
          :when (woot-char-visible char)
          :do (princ (woot-char-value char) out))))

(defun char-position (document woot-char)
  (position-by-char-id (remove-if-not #'woot-char-visible (document-sequence document))
                       (woot-char-id woot-char)))

(defun test-woot ()
  (let ((doc (make-document 1)))
    (let ((char1 (generate-insert doc 0 "a"))
          (char2 (generate-insert doc 1 "b"))
          (char3 (generate-insert doc 2 "c")))
      (assert (equal (mapcar #'woot-char-value (document-sequence doc))
                     '("start" "a" "b" "c" "end")))
      (assert (equal (mapcar #'woot-char-visible (document-sequence doc))
                     '(nil t t t nil)))
      (assert (char-id-equal (woot-char-next (document-start doc)) (woot-char-id char1)))
      (assert (char-id-equal (woot-char-next char1) (woot-char-id char2)))
      (assert (char-id-equal (woot-char-next char2) (woot-char-id char3)))
      (assert (char-id-equal (woot-char-next char3) (woot-char-id (document-end doc))))
      (assert (char-id-equal (woot-char-previous (document-end doc)) (woot-char-id char3)))
      (assert (char-id-equal (woot-char-previous char3) (woot-char-id char2)))
      (assert (char-id-equal (woot-char-previous char2) (woot-char-id char1)))
      (assert (char-id-equal (woot-char-previous char1) (woot-char-id (document-start doc))))
      (let ((char4 (generate-insert doc 1 "d")))
        (assert (equal (mapcar #'woot-char-value (document-sequence doc))
                       '("start" "a" "d" "b" "c" "end")))
        (assert (equal (mapcar #'woot-char-visible (document-sequence doc))
                       '(nil t t t t nil)))
        (assert (char-id-equal (woot-char-next (document-start doc)) (woot-char-id char1)))
        (assert (char-id-equal (woot-char-next char1) (woot-char-id char4)))
        (assert (char-id-equal (woot-char-next char4) (woot-char-id char2)))
        (assert (char-id-equal (woot-char-next char2) (woot-char-id char3)))
        (assert (char-id-equal (woot-char-next char3) (woot-char-id (document-end doc))))))))
