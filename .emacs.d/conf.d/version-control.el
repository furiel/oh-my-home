(require 'find-file-in-project)
(global-set-key (kbd "C-x f") 'find-file-in-project)

(advice-add
 'helm-grep-do-git-grep :around
 (lambda (orig &rest args)
   "Reverse sense of prefix arg."
   (let ((arg (car args)))
     (if arg
	 (setq args (cons nil (cdr args)))
       (setq args (cons '(4) (cdr args))))
     (apply orig args))))

(defun vc-root-dir ()
  (file-truename
   (let ((backend (vc-deduce-backend)))
     (and backend
	  (ignore-errors
	    (vc-call-backend backend 'root default-directory))))))
