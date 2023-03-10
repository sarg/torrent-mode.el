;;; torrent-mode.el --- Display torrent files in a tabulated view

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

(define-derived-mode torrent-mode tablist-mode
  "torrent"
  "Major mode for torrent files."

  ;; don't save it incidentally
  (setq buffer-file-name nil)

  (set-buffer-multibyte nil)
  (goto-char (point-min))
  (setq tabulated-list-format [("Idx" 4 t) ("Size" 8 t) ("Name" 80 t)]
        tabulated-list-entries
        (let* ((bencoding-dictionary-type 'hash-table)
               (data (bencoding-read))
               (info (gethash "info" data))
               (files (or (gethash "files" info)
                          (list info))))

          (seq-map-indexed
           (lambda (file index)
             (let* ((size (gethash "length" file))
                    (name (string-join (or (gethash "path" file)
                                           (list (gethash "name" file)))
                                       "/")))

               (list index (vector (format "%03d" index)
                                   (file-size-human-readable size)
                                   name))))
           files))

        tabulated-list-padding 3
        tabulated-list-sort-key (cons "Idx" nil))

  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(provide 'torrent-mode)
;;; torrent-mode.el ends here
