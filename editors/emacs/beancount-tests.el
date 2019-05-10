;;; beancount-test.el --- ERT for beancount-mode

;; Copyright 2019 Daniele Nicolodi <daniele@grinta.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.


(require 'ert)
(require 'beancount)

(ert-deftest beancount/smoke-001 ()
  :tags '(regress)
  (with-temp-buffer
    (beancount-mode)
    (font-lock-ensure)))
