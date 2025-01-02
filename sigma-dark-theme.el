;;; sigma-dark-theme.el --- SIGMA Dark Theme -*- lexical-binding: t -*-

(require 'sigma-theme-support)
(require 'theme-utils)

(deftheme sigma-dark "SIGMA Dark Theme")


(defun sigma-color (x y z)
  "Generate a normalized color using Oklab and scaling Y."
  (let (
        (scaled-y (/ (+ y 0) 24.0)))
    ;; (+/COLOR/get-normalized-oklab x scaled-y z)
    (+/COLOR/get-normalized-hpluv x scaled-y z)
    ;; (+/COLOR/get-normalized-hsluv x scaled-y z)
    ))

(let* ((fg 0.75)
       (bg 0.1)
      (color-scheme
       `(
         (sigma-color-default    . ,(sigma-color fg 16 0.6) )
         (sigma-color-background . ,(sigma-color bg  4 0.6) )
         (sigma-color-foreground . ,(sigma-color fg 16 0.9) )
         (sigma-color-highlight  . ,(sigma-color (+ fg .1)  4 0.9) )
         (sigma-color-subtle     . ,(sigma-color (- fg 0.1)  6 0.7) )
         (sigma-color-faded      . ,(sigma-color (- fg 0.2)  0 0.5) )
         (sigma-color-salient    . ,(sigma-color (+ fg 0.1)  4 0.9) )
         (sigma-color-popout     . ,(sigma-color (+ fg 0.1)  0 0.9) )
         (sigma-color-critical   . ,(sigma-color (+ fg 0.1)  0 0.9) )
         (sigma-color-strong     . ,(sigma-color (+ fg 0.1) 16 0.9) )
         
         (sigma-color-01    . ,(sigma-color fg  0 0.9) )
         (sigma-color-02    . ,(sigma-color fg  2 0.9) )
         (sigma-color-04    . ,(sigma-color fg  4 0.9) )
         (sigma-color-06    . ,(sigma-color fg  6 0.9) )
         (sigma-color-08    . ,(sigma-color fg  8 0.9) )
         (sigma-color-10    . ,(sigma-color fg 10 0.9) )
         (sigma-color-12    . ,(sigma-color fg 12 0.9) )
         (sigma-color-14    . ,(sigma-color fg 14 0.9) )
         (sigma-color-16    . ,(sigma-color fg 16 0.9) )
         (sigma-color-18    . ,(sigma-color fg 18 0.9) )
         (sigma-color-20    . ,(sigma-color fg 20 0.9) )
         (sigma-color-22    . ,(sigma-color fg 22 0.9) )
         )))

  (sigma-apply-theme 'sigma-dark color-scheme))

(provide-theme 'sigma-dark)
;;; sigma-dark-theme.el ends here
