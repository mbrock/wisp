;; -*- mode: wisp; fill-column: 64; -*-
;;
;; This file is part of Wisp.
;;
;; Wisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version
;; 3 of the License, or (at your option) any later version.
;;
;; Wisp is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General
;; Public License along with Wisp. If not, see
;; <https://www.gnu.org/licenses/>.
;;

;; (defun render-hello (data)
;;   (tag :div
;;     '((:class "bg-blue-400 text-yellow-50/80 border rounded p-2"))
;;     (text "Hello, world!")
;;     (tag :div '((:class "bg-blue-600 border rounded p-2"))
;;       (text "Yeah!"))))

;; (compile! #'render-hello)

;; (defun hello-world ()
;;   (dom-patch!
;;    (query-selector "#wisp-app")
;;    (make-callback 'render-hello) nil))

;; (with-simple-error-handler ()
;;   (hello-world))
