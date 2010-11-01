;;; -*- Mode: Emacs-Lisp; -*-

;;;; unipoint.el - a simple way to insert unicode characters by TeX name
;; 
;; This file is NOT part of GNU Emacs
;;
;; Copyright (c) 2010, Andrew Gwozdziewycz <git@apgwoz.com>
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version. This is
;; distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details. You
;; should have received a copy of the GNU General Public License along with 
;; Emacs; see the file `COPYING'. If not, write to the Free Software 
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;; How to use:
;; Add this file to your load-path
;; (require 'unipoint)
;; turn on unipoint-mode
;; chord 

(defconst *unipoint-table*
  '(("pm" "±")
    ("to" "→")
    ("Rightarrow" "⇒")
    ("Leftrightarrow" "⇔")
    ("forall" "∀")
    ("partial" "∂")
    ("exists" "∃")
    ("emptyset" "∅")
    ("nabla" "∇")
    ("in" "∈")
    ("notin" "∉")
    ("prod" "∏")
    ("sum" "∑")
    ("surd" "√")
    ("infty" "∞")
    ("wedge" "∧")
    ("vee" "∨")
    ("cap" "∩")
    ("cup" "∪")
    ("int" "∫")
    ("approx" "≈")
    ("neq" "≠")
    ("equiv" "≡")
    ("leq" "≤")
    ("geq" "≥")
    ("subset" "⊂")
    ("supset" "⊃")
    ("cdot" "⋅")
    ("circ" "°")
    ("times" "×")
    ("lfloor" "⌊")
    ("rfloor" "⌋")
    ("lceil" "⌈")
    ("rceil" "⌉")
    ("Gamma" "Γ")
    ("Delta" "Δ")
    ("Theta" "Θ")
    ("Lambda" "Λ")
    ("Xi" "Ξ")
    ("Pi" "Π")
    ("Sigma" "Σ")
    ("Upsilon" "Υ")
    ("Phi" "Φ")
    ("Psi" "Ψ")
    ("Omega" "Ω")
    ("alpha" "α") 
    ("beta" "β")
    ("gamma" "γ")
    ("delta" "δ")
    ("epsilon" "ε")
    ("zeta" "ζ")
    ("eta" "η")
    ("theta" "θ")
    ("iota" "ι")
    ("kappa" "κ")
    ("lambda" "λ")
    ("mu" "μ")
    ("nu" "ν")
    ("xi" "ξ")
    ("pi" "π")
    ("rho" "ρ")
    ("varsigma" "ς")
    ("sigma" "σ")
    ("tau" "τ")
    ("upsilon" "υ")
    ("phi" "φ")
    ("chi" "χ")
    ("psi" "ψ")
    ("omega" "ω")
    ))

(defun unipoint-insert ()
  "Inserts at point, a unicode codepoint by name"
  (interactive)
  (let* ((ins (completing-read "\\" 
                               (mapcar 'identity *unipoint-table*) nil nil))
         (ent (assoc-string ins *unipoint-table*)))
    (if ent
        (insert (cadr ent))
      (insert (concat "\\" ins)))))

(define-minor-mode unipoint-mode
  "Toggle Unipoint mode."
  ;; initial value
  nil
  ;; indicator
  " UP"
  ;; keybindings
  '(("\C-\\" . unipoint-insert)))

(provide 'unipoint)
