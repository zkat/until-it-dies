(in-package :uid)

#+(or allegro clisp lispworks openmcl)
(defvar *finalizers*
  (cl:make-hash-table :test 'eq
                      #+allegro :weak-keys #+:allegro t
                      #+(or clisp openmcl) :weak
                      #+lispworks :weak-kind
                      #+(or clisp openmcl lispworks) :key)
  "Weak hashtable that holds registered finalizers.")

#+corman
(progn
  (defvar *finalizers* '()
    "Weak alist that holds registered finalizers.")

  (defvar *finalizers-cs* (threads:allocate-critical-section)))

#+lispworks
(progn
  (hcl:add-special-free-action 'free-action)
  (defun free-action (object)
    (let ((finalizers (gethash object *finalizers*)))
      (unless (null finalizers)
        (mapc #'funcall finalizers)))))

(defun finalize (object function)
  "Pushes a new FUNCTION to the OBJECT's list of
   finalizers. FUNCTION should take no arguments. Returns OBJECT.

   For portability reasons, FUNCTION should not attempt to look
   at OBJECT by closing over it because, in some lisps, OBJECT
   will already have been garbage collected and is therefore not
   accessible when FUNCTION is invoked."
  #+(or cmu scl) (ext:finalize object function)
  #+sbcl (sb-ext:finalize object function)
  #+ecl (let ((next-fn (ext:get-finalizer object)))
          (ext:set-finalizer
           object (lambda (obj)
                    (declare (ignore obj))
                    (funcall function)
                    (when next-fn
                      (funcall next-fn nil)))))
  #+allegro
  (progn
    (push (excl:schedule-finalization
           object (lambda (obj) (declare (ignore obj)) (funcall function)))
          (gethash object *finalizers*))
    object)
  #+clisp
  ;; The CLISP code used to be a bit simpler but we had to workaround
  ;; a bug regarding the interaction between GC and weak hashtables.
  ;; See <http://article.gmane.org/gmane.lisp.clisp.general/11028>
  ;; and <http://article.gmane.org/gmane.lisp.cffi.devel/994>.
  (multiple-value-bind (finalizers presentp)
      (gethash object *finalizers* (cons 'finalizers nil))
    (unless presentp
      (setf (gethash object *finalizers*) finalizers)
      (ext:finalize object (lambda (obj)
                             (declare (ignore obj))
                             (mapc #'funcall (cdr finalizers)))))
    (push function (cdr finalizers))
    object)
  #+openmcl
  (progn
    (ccl:terminate-when-unreachable
     object (lambda (obj) (declare (ignore obj)) (funcall function)))
    ;; store number of finalizers
    (incf (gethash object *finalizers* 0))
    object)
  #+corman
  (flet ((get-finalizers (obj)
           (assoc obj *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
    (threads:with-synchronization *finalizers-cs*
      (let ((pair (get-finalizers object)))
        (if (null pair)
            (push (list (ccl:make-weak-pointer object) function) *finalizers*)
            (push function (cdr pair)))))
    (ccl:register-finalization
     object (lambda (obj)
              (threads:with-synchronization *finalizers-cs*
                (mapc #'funcall (cdr (get-finalizers obj)))
                (setq *finalizers*
                      (delete obj *finalizers*
                              :test #'eq :key #'ccl:weak-pointer-obj)))))
    object)
  #+lispworks
  (progn
    (let ((finalizers (gethash object *finalizers*)))
      (unless finalizers
        (hcl:flag-special-free-action object))
      (setf (gethash object *finalizers*)
            (cons function finalizers)))
    object))

(defun cancel-finalization (object)
  "Cancels all of OBJECT's finalizers, if any."
  #+cmu (ext:cancel-finalization object)
  #+scl (ext:cancel-finalization object nil)
  #+sbcl (sb-ext:cancel-finalization object)
  #+ecl (ext:set-finalizer object nil)
  #+allegro
  (progn
    (mapc #'excl:unschedule-finalization
          (gethash object *finalizers*))
    (remhash object *finalizers*))
  #+clisp
  (multiple-value-bind (finalizers present-p)
      (gethash object *finalizers*)
    (when present-p
      (setf (cdr finalizers) nil))
    (remhash object *finalizers*))
  #+openmcl
  (let ((count (gethash object *finalizers*)))
    (unless (null count)
      (dotimes (i count)
        (ccl:cancel-terminate-when-unreachable object))))
  #+corman
  (threads:with-synchronization *finalizers-cs*
    (setq *finalizers*
          (delete object *finalizers* :test #'eq :key #'ccl:weak-pointer-obj)))
  #+lispworks
  (progn
    (remhash object *finalizers*)
    (hcl:flag-not-special-free-action object)))
