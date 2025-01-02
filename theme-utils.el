;;; theme-utils.el --- Utility functions for theme customization -*- lexical-binding: t; -*-

(require 'color)
(require 'hsluv)
(require 'cl-lib)

(defun +/UTIL/linear-ascent-descent-value (min-val max-val x)
  "Return the value at position X (normalized to [0, 1]) on a linear ascent-descent curve
that starts at MIN-VAL, peaks at MAX-VAL, and descends back to MIN-VAL."
  (unless (and (numberp min-val) (numberp max-val) (numberp x))
    (error "All arguments must be numbers"))
  (unless (and (>= x 0.0) (<= x 1.0))
    (error "X must be in the range [0, 1]"))
  (if (<= x 0.5)
      ;; Ascent phase
      (let ((normalized-x (/ x 0.5))) ;; Normalize x to [0, 1] for ascent
        (+ min-val (* normalized-x (- max-val min-val))))
    ;; Descent phase
    (let ((normalized-x (/ (- x 0.5) 0.5))) ;; Normalize x to [0, 1] for descent
      (- max-val (* normalized-x (- max-val min-val))))))

(defun +/UTIL/exponential-ascent-descent-value (min-val max-val x exponent)
  "Return the value at position X (normalized to [0, 1]) on an exponential ascent-descent curve.
The curve starts at MIN-VAL, peaks at MAX-VAL, and descends back to MIN-VAL, using EXPONENT for steepness."
  (unless (and (numberp min-val) (numberp max-val) (numberp x) (numberp exponent))
    (error "All arguments must be numbers"))
  (unless (and (>= x 0.0) (<= x 1.0))
    (error "X must be in the range [0, 1]"))
  (+(*
   (+ (* -4 (expt (- x 0.5) exponent)) 1)
    (- max-val min-val)
   )min-val)
  )

(defun +/UTIL/calculate-luminance (hex-color)
  "Calculate the approximate perceptual luminance of a HEX-COLOR.
The luminance is calculated using the relative luminance formula."
  (let* ((rgb (color-name-to-rgb hex-color)) ;; Convert hex to RGB values
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         ;; Adjust values for luminance calculation
         (r-lum (if (<= r 0.03928) (/ r 12.92) (expt (/ (+ r 0.055) 1.055) 2.4)))
         (g-lum (if (<= g 0.03928) (/ g 12.92) (expt (/ (+ g 0.055) 1.055) 2.4)))
         (b-lum (if (<= b 0.03928) (/ b 12.92) (expt (/ (+ b 0.055) 1.055) 2.4))))
    ;; Combine RGB values into luminance
    (+ (* 0.2126 r-lum) (* 0.7152 g-lum) (* 0.0722 b-lum))))

(defun +/UTIL/hsluv-luminance-to-lightness (luminance)
  "Approximate the HSLuv L value (lightness) for a given luminance (0 to 1)."
  (if (or (< luminance 0) (> luminance 1))
      (error "Luminance must be in the range [0, 1]")
    (* 100 (sqrt luminance)))) ;; Scale by 100 to match HSLuv's L range


(defun +/COLOR/get-normalized-hsluv (x y z)
  "Return a normalized HSLuv hex color based on X (saturation) and Y (hue/lightness)."
  ;; (message (format "Oklab %f %f %f" x y z  ))
  (let* (
         (x( if( > x 1.0) (mod x 1.0) x))
         (y( if ( > y 1.0) (mod y 1.0) y))
         (z( if ( > z 1.0) 1.0 z))
         (hue (* y 360)) ;; Hue from 0 to 360
         (lightness
          ;; (+/UTIL/hsluv-luminance-to-lightness x)
          (* x 100)
          ) ;; Lightness from 0 to 100
         ;; (saturation(+/UTIL/exponential-ascent-descent-value 0 (* z 100) x 6))
         (saturation(+/UTIL/linear-ascent-descent-value 0 (* z 100) x))
         (rgb (hsluv-hsluv-to-rgb (list hue saturation lightness))) ;; Convert to RGB
         (hex (format "#%02x%02x%02x"
                      (round (* 255 (nth 0 rgb))) ;; Red
                      (round (* 255 (nth 1 rgb))) ;; Green
                      (round (* 255 (nth 2 rgb)))))) ;; Blue
    hex)) ;; Return the hex color

(defun +/COLOR/get-normalized-hpluv (x y z)
  "Return a normalized HSLuv hex color based on X (saturation) and Y (hue/lightness)."
  ;; (message (format "Oklab %f %f %f" x y z  ))
  (let* (
         (x( if( > x 1.0) (mod x 1.0) x))
         (y( if ( > y 1.0) (mod y 1.0) y))
         (z( if ( > z 1.0) 1.0 z))
         (hue (* y 360)) ;; Hue from 0 to 360
         (lightness
          ;; (+/UTIL/hsluv-luminance-to-lightness x)
          (* x 100)
          ) ;; Lightness from 0 to 100
         ;; (saturation(+/UTIL/exponential-ascent-descent-value 0 (* z 100) x 6))
         (saturation(+/UTIL/linear-ascent-descent-value 0 (* z 100) x))
         (rgb (hsluv-hpluv-to-rgb (list hue saturation lightness))) ;; Convert to RGB
         (hex (format "#%02x%02x%02x"
                      (round (* 255 (nth 0 rgb))) ;; Red
                      (round (* 255 (nth 1 rgb))) ;; Green
                      (round (* 255 (nth 2 rgb)))))) ;; Blue
    hex)) ;; Return the hex color

(defun +/COLOR/get-normalized-oklab (x y z)
  "Return a normalized HSLuv hex color based on X (saturation) and Y (hue/lightness)."
  ;; (message (format "Oklab %f %f %f" x y z  ))
  (let* (
         (x( if ( > x 1.0) (mod x 1.0) x))
         (y( if ( > y 1.0) (mod y 1.0) y))
         (z( if ( > z 1.0) 1.0 z))
         (lightness 
          ;; (* (+/UTIL/hsluv-luminance-to-lightness x) 1)
          (* x 100)
          ) ;; Lightness from 0 to 100
         ;; (chroma(+/UTIL/exponential-ascent-descent-value 0 (* z 25) x 2))
         (chroma(+/UTIL/linear-ascent-descent-value 0 (* z 29 ) x))
         ;; (chroma 0.4)
         (h (* y 360)) ;; Hue from 0 to 360
         (hue-radians (* h (/ pi 180))) ;; Convert hue from degrees to radians
          (lab (color-lch-to-lab lightness chroma hue-radians)) ;; Convert LCH to Lab
          ;; (l (* (nth 0 lab) 0.01))
          ;; (a (* (nth 1 lab) .0032))
          ;; (b (* (nth 2 lab) .0032))
          ;; (rgb (apply #'color-oklab-to-srgb (list l a b) ))
          (l (nth 0 lab))
          (a (nth 1 lab))
          (b (nth 2 lab))
          (rgb (apply #'color-lab-to-srgb (list l a b) ))
          
          (hex (format "#%02x%02x%02x"
                      (round (* 255 (nth 0 rgb))) ;; Red
                      (round (* 255 (nth 1 rgb))) ;; Green
                      (round (* 255 (nth 2 rgb)))))) ;; Blue
    hex)) ;; Return the hex color

(defun +/THEME/set-syntax-colors (lightness chroma offset)
  "Generate colors with the given LIGHTNESS and CHROMA, and set them for syntax highlighting using `custom-set-faces`.
The colors start with the grey value and proceed in ascending hue (0 to 1 in 12 steps)."
  (let(
       (faded  1.0)
       (subtle .7)
       (salient .0)
       (warning .0)
       )
    (custom-set-faces
     `(default                           ((t
                                           (
                                            :foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset   (/ 0.0 12))(* chroma subtle))
                                            :background ,(+/COLOR/get-normalized-hsluv (* lightness .1)(+ offset   (/ 6.0 12))(* chroma subtle))
                                                        )
                                           )))
     `(window-divider                    ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  6.0 12)) chroma)))))
     `(font-lock-type-face               ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  1.0 12)) chroma)))))
     `(font-lock-function-name-face      ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  2.0 12)) chroma)))))
     `(font-lock-variable-name-face      ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  3.0 12)) chroma)))))
     `(font-lock-keyword-face            ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  4.0 12)) chroma)))))
     `(font-lock-string-face             ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  5.0 12)) chroma)))))
     `(font-lock-warning-face            ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  6.0 12)) chroma )))))
     `(font-lock-doc-face                ((t (:foreground ,(+/COLOR/get-normalized-hsluv (* lightness faded)(+ offset (/ 7.0 12)) (* chroma faded))))))
     `(font-lock-comment-face            ((t (:foreground ,(+/COLOR/get-normalized-hsluv (* lightness faded)(+ offset (/ 8.0 12)) (* chroma faded))))))
     `(font-lock-comment-delimiter-face  ((t (:foreground ,(+/COLOR/get-normalized-hsluv (* lightness faded)(+ offset (/ 8.0 12)) (* chroma faded))))))
     `(font-lock-constant-face           ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/  9.0 12)) chroma)))))
     `(font-lock-builtin-face            ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/ 10.0 12)) chroma)))))
     `(font-lock-preprocessor-face       ((t (:foreground ,(+/COLOR/get-normalized-hsluv lightness (+ offset (/ 11.0 12)) chroma)))))
              )
            )
    )                          

(defun +/UTIL/generate-interval-values (n)
  "Generate a list of values by dividing 1.0 into N equal intervals."
  (let ((step (/ 1.0 n)))
    (cl-loop for i from 0 below n
             collect (* i step))))

(defun +/UTIL/example-font-lock-faces ()
  "Create a buffer showing all the font-lock faces with example text."
  (let ((buffer (get-buffer-create "*Font-Lock Faces Example*")))
    (with-current-buffer buffer
      (erase-buffer) ;; Clear the buffer
      (insert (propertize "Default face\n" 'face 'default))
      (insert (propertize "Warning face\n" 'face 'font-lock-warning-face))
      (insert (propertize "Function name face\n" 'face 'font-lock-function-name-face))
      (insert (propertize "Variable name face\n" 'face 'font-lock-variable-name-face))
      (insert (propertize "Keyword face\n" 'face 'font-lock-keyword-face))
      (insert (propertize "Comment face\n" 'face 'font-lock-comment-face))
      (insert (propertize "Comment delimiter face\n" 'face 'font-lock-comment-delimiter-face))
      (insert (propertize "Type face\n" 'face 'font-lock-type-face))
      (insert (propertize "Constant face\n" 'face 'font-lock-constant-face))
      (insert (propertize "Built-in face\n" 'face 'font-lock-builtin-face))
      (insert (propertize "Preprocessor face\n" 'face 'font-lock-preprocessor-face))
      (insert (propertize "String face\n" 'face 'font-lock-string-face))
      (insert (propertize "Doc face\n" 'face 'font-lock-doc-face))
      ;; Display the buffer
      (display-buffer buffer))))

(defun hex-to-brightness (hex-color)
  "Calculate the brightness of a HEX-COLOR using the formula:
   brightness = sqrt(0.299 * R^2 + 0.587 * G^2 + 0.114 * B^2)."
  (let* ((rgb (color-name-to-rgb hex-color)) ;; Convert HEX to RGB (normalized 0-1)
         (r (* 255 (nth 0 rgb))) ;; Scale red to 0-255
         (g (* 255 (nth 1 rgb))) ;; Scale green to 0-255
         (b (* 255 (nth 2 rgb))) ;; Scale blue to 0-255
         (brightness (sqrt (+ (* 0.299 (expt r 2)) 
                              (* 0.587 (expt g 2)) 
                              (* 0.114 (expt b 2)))))) ;; Brightness formula
    brightness))

(defun +/UTIL/generate-normalized-color-grid (buffer-name)
  "Generate a grid of colors based on lightness (L), chroma (C), and hue (H) using HSLuv.
Fill an Emacs buffer with the output."
  (let* (
         (buffer (get-buffer-create buffer-name))) ;; Create or get buffer
    (with-current-buffer buffer
      (erase-buffer) ;; Clear the buffer
      (insert (format "\nGenerated Color Grid Using HSLuv\n"))
      (cl-loop for i from 0 to 1 by .025 do
               (insert (format "Hue %03d: " (* i 360)))
               (cl-loop for j from 0 to 1 by .11 do
                        (let* ((hex (+/COLOR/get-normalized-hsluv j i .5))
                               (lum (+/UTIL/calculate-luminance hex)))
                          (insert (format "%s %.2f|" hex lum))))
               (insert "\n")) ;; Add a newline between rows
      (display-buffer buffer)))) ;; Show buffer

(defun +/UTIL/generate-normalized-color-grid-html (identifier)
  "Generate an HTML file with a grid of colors based on lightness (L), chroma (C), and hue (H) using HSLuv.
Each cell displays a color swatch with the hex code underneath."
  (let ((css "
<style>
  body {
    font-family: Arial, sans-serif;
    font-size: 14px;
    margin: 20px;
  }
  .color-grid {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
  }
  .color-cell {
    width: 60px;
    height: auto;
    text-align: center;
    border: 1px solid #ccc;
    border-radius: 4px;
    box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);
  }
  .swatch {
    width: 100%;
    height: 60px;
    border-bottom: 1px solid #bbb;
  }
  .hex-code {
    font-size: 8px;
    padding: 4px;
    color: #333;
  }
 ul {
      display: flex; /* Makes the <ul> a flex container */
      flex-wrap: wrap; /* Allows <li> to wrap to the next line if needed */
      list-style-type: none; /* Remove bullets */
      margin: 0;
      padding: 0;
      width: 100%; /* Ensure <ul> takes the full width of the row */
      // border-bottom: 1px solid #ddd; /* Optional: Add a separator between rows */
    }

    ul:last-child {
      border-bottom: none; /* Remove border for the last <ul> */
    }

    ul li {
      display: inline-block; /* Make <li> elements inline */
      padding: 5px; /* Add spacing around each <li> */
      flex: 1; /* Distribute space equally between <li> elements */
      text-align: center; /* Optional: Center text inside <li> */
     // border-right: 1px solid #ddd; /* Optional: Add a separator between <li> */
    }

    ul li:last-child {
      border-right: none; /* Remove the border for the last <li> */
    }
</style>
")
        (html-header "<!DOCTYPE html><html><head><title>Color Grid</title>")
        (html-footer "</div></body></html>")
        (file (expand-file-name (format "color-grid-%s.html" identifier)))
        (rows 40)
        (cols 10))
    (with-temp-file file
      ;; Write the HTML header and CSS
      (insert html-header)
      (insert css)
      (insert "</head><body>")
      (insert (format "<h1>Generated Color Grid Using %s</h1>" identifier))
      (insert "<div class='color-grid'>")
      ;; Generate the color grid
      (cl-loop for i from 0 to rows do
               (insert "<ul>")
               (let (
                     (y (/ i (float rows)))
                     (z 1.0)
                     )
                 (cl-loop for j from 0 to cols do
                          (insert "<li>")
                          (let* ((x (/ j (float cols)))
                                 ;; (hex (+/COLOR/get-normalized-hsluv x y z ))
                                 ;; (hex (+/COLOR/get-normalized-oklab x y z ))
                                 (hex (+/COLOR/get-normalized-hpluv x y z ))
                                 (lum (+/UTIL/calculate-luminance hex))
                                 (brightness (hex-to-brightness hex))
                                 )
                            (insert (format "
<div class='color-cell'>
  <div class='swatch' style='background-color:%s;'></div>
  <div class='hex-code'>%s %.0f %.2f</div>
</div>" hex hex brightness lum)))
                          (insert "</li>")
                          )
                 )
               (insert "</ul>")
               )
      ;; Close HTML structure
      (insert html-footer))
    ;; Notify user
    (message "HTML file generated: %s" file)))


(provide 'theme-utils)

;; Example Usage:
;; (+/THEME/set-syntax-colors 0.8 0.6 (/ 7.5 12) ) ;; Lightness = 0.1, Chroma = 0.7
;; (+/UTIL/example-font-lock-faces)
;; (+/UTIL/generate-normalized-color-grid "*Color Grid*")
;; (+/UTIL/generate-normalized-color-grid-html "hpluv")
;;; theme-utils.el ends here
