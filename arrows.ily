%%% "arrows.ily %%%

#(define-markup-command 
   (arrow layout props 
     type ;; "open" "close" "huron" "long" "large" "wings" "relief"
     fletching? ;; #t or #f
     axis ;; X or Y
     direction ;; RIGHT or LEFT, UP or DOWN, 
     a-length ;; any number >= #0
     boldness) ;; #0 < any number < #1
   (string? boolean? ly:dir? ly:dir? number? number?)
  "Draw arrows with variable length & boldness."
  (define fletching #t)  
  (interpret-markup layout props
    (cond 
     

 
;; "long" optimal thickness #0.07
      ((and (string=? type "long")(and (eq? axis X)(eq? direction 1)))
            (markup 
             (#:override (cons (quote filled) #t)
              (#:path boldness
               `((moveto    0.00  0.00)
                 (lineto   -1.80 -0.60)
                 ;(curveto  -2.40 -0.40 -2.35 -0.15 -2.35 -0.05)
                 ;(lineto   ,(* a-length -1) -0.05)
                 ;(lineto   ,(* a-length -1)  0.05)
                 (lineto   -1.80  0.00)
                 ;(curveto  -2.35  0.10 -2.40  0.40 -1.80  0.60)
                 ;(lineto   -2.35  0.10)
                 ;(lineto   -2.40  0.40)
                 (lineto   -1.80  0.60)
                 ;(lineto  -2.35  0.10 )
                 (lineto    0.00  0.00)
                 (closepath))))))
     
   
      
 
      
;; "large" optimal thickness #0.07
 
          
      (else (ly:error "Arrows' parameter(s) do not fit")))))


%% slashed grace notes

slash =
#(define-music-function (parser location ang stem-fraction protrusion)
   (number? number? number?)
   (remove-grace-property 'Voice 'Stem 'direction) ; necessary?
   #{
     \once \override Stem #'stencil =
     #(lambda (grob)
       (let* ((X-parent (ly:grob-parent grob X))
              (is-rest? (ly:grob? (ly:grob-object X-parent 'rest))))
         (if is-rest?
             empty-stencil
             (let* ((ang (degrees->radians ang))
                    ; We need the beam and its slope so that slash will
                    ; extend uniformly past the stem and the beam
                    (beam (ly:grob-object grob 'beam))
                    (beam-X-pos (ly:grob-property beam 'X-positions))
                    (beam-Y-pos (ly:grob-property beam 'positions))
                    (beam-slope (/ (- (cdr beam-Y-pos) (car beam-Y-pos))
                                   (- (cdr beam-X-pos) (car beam-X-pos))))
                    (beam-angle (atan beam-slope))
                    (stem-Y-ext (ly:grob-extent grob grob Y))
                    ; Stem.length is expressed in half staff-spaces
                    (stem-length (/ (ly:grob-property grob 'length) 2.0))
                    (dir (ly:grob-property grob 'direction))
                    ; if stem points up. car represents segment of stem
                    ; closest to notehead; if down, cdr does
                    (stem-ref (if (= dir 1) (car stem-Y-ext) (cdr stem-Y-ext)))
                    (stem-segment (* stem-length stem-fraction))
                    ; Where does slash cross the stem?
                    (slash-stem-Y (+ stem-ref (* dir stem-segment)))
                    ; These are values for the portion of the slash that
                    ; intersects the beamed group.
                    (dx (/ (- stem-length stem-segment)
                           (- (tan ang) (* dir beam-slope))))
                    (dy (* (tan ang) dx))
                    ; Now, we add in the wings
                    (protrusion-dx (* (cos ang) protrusion))
                    (protrusion-dy (* (sin ang) protrusion))
                    (x1 (- protrusion-dx))
                    (y1 (- slash-stem-Y (* dir protrusion-dy)))
                    (x2 (+ dx protrusion-dx))
                    (y2 (+ slash-stem-Y
                           (* dir (+ dy protrusion-dy))))
                    (th (ly:staff-symbol-line-thickness grob))
                    (stil (ly:stem::print grob)))

              (ly:stencil-add
                stil
                (make-line-stencil th x1 y1 x2 y2))))))
   #})

slashI = {
  \slash 50 0.6 1.0
}