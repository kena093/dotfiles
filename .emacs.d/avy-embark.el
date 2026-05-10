(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer))
  :config
  (defun my/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (let ((start (line-beginning-position))
            (end (line-beginning-position 2)))
        (copy-region-as-kill start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'my/avy-action-embark
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line))

(use-package embark
  :ensure t
  :bind (("C-c e" . embark-act)))

(provide 'avy-embark)
