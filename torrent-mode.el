;;; torrent-mode.el --- Display torrent files in a tabulated view  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by Sergey Trofimov

;; Author: Sergey Trofimov <sarg@sarg.org.ru>
;; Version: 0.1
;; URL: https://github.com/sarg/torrent-mode.el
;; Package-Requires: ((emacs "26.1") (tablist "1.0") (bencoding "1.0"))

;;; Commentary:
;; This package displays torrent files using tablist-mode.

;;; Code:
(require 'tablist)
(require 'bencoding)

;;;###autoload
(define-derived-mode torrent-mode tablist-mode
  "torrent"
  "Major mode for torrent files."

  ;; don't save it incidentally
  (setq-local original-buffer-file-name buffer-file-name)
  (setq buffer-file-name nil)

  (goto-char (point-min))
  (let* ((bencoding-dictionary-type 'hash-table)
         (data (progn (set-buffer-multibyte nil) (bencoding-read) ))
         (info (gethash "info" data))
         (files (or (gethash "files" info) (list info)))
         (sortfun
          (lambda (n)
            (lambda (A B) (value< (get-text-property 0 'sortval (aref (nth 1 A) n))
                                  (get-text-property 0 'sortval (aref (nth 1 B) n)))))))

    (setq tabulated-list-entries
          (seq-map-indexed
           (lambda (file index)
             (let* ((size (gethash "length" file))
                    (name (decode-coding-string
                           (string-join (or (gethash "path" file)
                                            (list (gethash "name" file)))
                                        "/")
                           'utf-8)))

               (list index (vector (propertize (number-to-string index) 'sortval index)
                                   (propertize (file-size-human-readable size) 'sortval size)
                                   name))))
           files))

    (setq tabulated-list-format (vector `("Idx" 4 ,(funcall sortfun 0) . (:right-align t))
                                        `("Size" 6 ,(funcall sortfun 1) . (:right-align t))
                                        `("Name" 80 t))
          tabulated-list-padding 3
          tabulated-list-sort-key (cons "Idx" nil)))

  (set-buffer-multibyte 't)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(provide 'torrent-mode)
;;; torrent-mode.el ends here
