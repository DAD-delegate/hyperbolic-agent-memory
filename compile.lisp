;;;; Compiler for a 4-cell prefix ISA. Every form is (op a b c).
;;;; Unknown length or unknown op is an error. No macros, no keywords.

(defparameter *ham-ops*
  '((root . 4) (add . 4) (save . 4) (load . 4) (check . 4)))

(defun ham-op-ok (form)
  (and (consp form)
       (= (length form) 4)
       (symbolp (car form))
       (assoc (car form) *ham-ops*)))

(defun ham-check (s)
  (dolist (n (ham-nodes s))
    (let ((parent (ham-parent n)))
      (if (= parent 0)
          (when (> (ham-norm (ham-xy n)) +ham-eps+)
            (error "root not origin"))
          (let* ((p (ham-find s parent))
                 (d (ham-dist (ham-xy p) (ham-xy n))))
            (when (<= (ham-norm (ham-xy n)) (ham-norm (ham-xy p)))
              (error "norm order"))
            (when (not (> d 0.0d0))
              (error "bad dist"))))))
  (format t "M0-OK~%")
  (format t "~a" (ham-brief s))
  s)

(defun ham-emit (s form)
  (let ((op (car form)))
    (cond ((eq op 'root)
           (ham-root s (nth 1 form)))
          ((eq op 'add)
           (ham-add s (nth 1 form) (nth 2 form) (nth 3 form)))
          ((eq op 'save)
           (ham-save s (nth 1 form)))
          ((eq op 'load)
           (ham-load (nth 1 form)))
          ((eq op 'check)
           (ham-check s))
          (t (error "bad op")))))

(defun ham-compile-file (path)
  (let ((s (ham-empty)))
    (with-open-file (in path :direction :input)
      (tagbody
       again
        (let ((form (read in nil :eof)))
          (when (eq form :eof)
            (return-from ham-compile-file s))
          (unless (ham-op-ok form)
            (error "not a 4-cell op"))
          (setq s (ham-emit s form)))
        (go again)))))
