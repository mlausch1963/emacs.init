;;;; mla-tools.el --- Private functions -*- lexical-binding: t -*-

;;; Commentary:
;; private Emacs functions and extensions

;;; Code:

;; 1. The Package Prefix (Your Namespace)
;; By official convention, use `mla-tools-` for public functions
;; and `mla-tools--` for internal helper functions.


(require 'thingatpt)

(define-prefix-command 'mla-tools-search-map)
(global-set-key (kbd "C-s") 'mla-tools-search-map)
(define-key mla-tools-search-map (kbd "C-s") 'isearch-forward)

(with-eval-after-load 'swiper
  (define-key mla-tools-search-map (kbd "s") 'swiper))

(provide 'mla-tools-searchkeys)


(defun mla-tools-quote-word-at-point ()
  "Quote the whitespace-delimited word at point using the current syntax table.
Uses single quotes for `python-mode` and double quotes otherwise.
Avoids duplicating existing leading or trailing quotes."
  (interactive)
  (let* ((choice-quote (if (derived-mode-p 'python-mode) "'" "\""))
         (target-char (string-to-char choice-quote))
         (orig-point (point))

         ;; 1. Find the boundaries based on the syntax table's whitespace class
         (beg (save-excursion
                (skip-syntax-backward "^ ")
                (point)))
         (end (save-excursion
                (skip-syntax-forward "^ ")
                (point))))

    ;; Only act if the cursor is actually on a word (beg != end)
    (when (/= beg end)
      (let ((has-leading (eq (char-after beg) target-char))
            ;; (1- end) checks the actual last character inside the boundaries
            (has-trailing (eq (char-after (1- end)) target-char)))

        ;; 2. Insert trailing quote first so 'beg' index doesn't shift
        (unless has-trailing
          (goto-char end)
          (insert choice-quote))

        (unless has-leading
          (goto-char beg)
          (insert choice-quote))

        ;; 3. Restore cursor position dynamically
        (goto-char (+ orig-point (if has-leading 0 1)))))))

(provide 'mla-tools)

;;; mla-tools.el ends here
