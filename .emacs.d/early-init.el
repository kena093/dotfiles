(setq gc-cons-threshold most-positive-fixnum)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))