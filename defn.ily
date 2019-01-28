\include "arrows.ily"
				% \stopStaff and \startStaff are for changing staff properties, not turning the staff off,
% The StaffSymbol and Barline Transparencies are for that
invs =
#(define-music-function
  (parser location)
  ()
  #{
    \stopStaff
    \override Staff.StaffSymbol.transparent = ##t
    % \override Staff.BarLine.transparent = ##t
				%       \override Staff.Clef.transparent = ##t
  \override Staff.Clef.break-visibility = #all-invisible
    \startStaff

  #}

  )

% make the barline and staffsymbol visible again
notinvs =
#(define-music-function
  (parser location)
  ()
  #{
    \stopStaff
    \override Staff.StaffSymbol.transparent = ##f
    \override Staff.BarLine.transparent = ##f
    \startStaff
  #}

  )




#(define (number-or-boolean? arg)
   (or (number? arg)
       (boolean? arg)))

% #(define-markup-command (arrow layout props ang length filled)
%    (number? number-or-boolean? boolean?)
%    #:category graphic
%    "Draw an arrow of length @var{length}.
% Use filled head if @var{filled} is true.
% Rotate by @var{ang} degrees; default points to the right.
% In order to draw an arrowhead only, use @code{##f} for @var{length}.
% @lilypond[verbatim,quote]
% \\markup {
%   \\arrow #0 #3 ##f
%   \\arrow #-60 #5 ##t
%   \\arrow #90 ##f ##t
% }
% @end lilypond
% "
%    (let* ((head (markup #:arrow-head X 1 filled))
%           (arro (if length
%                     (markup
%                      #:combine
%                      head
%                      #:override '(thickness . 1.5)
%                      #:draw-line (cons (- length) 0))
%                     head)))
%      (interpret-markup layout props
%        (markup
%         #:rotate ang
%         arro))))

caesuraMk = \markup {
  \translate #'(0 . 0) { \musicglyph #"scripts.caesura.straight" }
}

arrowCaesura = \markup{
  \translate #'(-3 . 0.5) {
    \magnify #0.7 {
      #(make-musicglyph-markup "scripts.caesura.straight")
    }
  }
}

arrowBreathe = \markup{
  \translate #'(-3 . 1.1) {
    \magnify #0.7 {
      #(make-musicglyph-markup "scripts.rcomma")
    }
  }
}

fnt =#'(font-name . "Century Schoolbook Regular")
ital = #'(font-name . "TeX Gyre Schola Italic")

cmp = \markup {
  \override \fnt
  \smallCaps
  "Nicholas Shaheed"
}


ttl = \markup {
  \override \fnt
  \override #'(font-size . 8)
  \smallCaps
  "Fijeo Wange"
}

subttl = \markup {
  \override \fnt
  \smallCaps
   "for Two Violins"
}

yr = \markup {
  \override \fnt
  \smallCaps
  2018
}

\defineBarLine "[" #'("" "[" "")
\defineBarLine "]" #'("]" "" "")
\defineBarLine "|-nothing" #'("" "" "")
\defineBarLine "|-empty" #'("|" "" "")
\defineBarLine "||-cell" #'("||" "[" "")
\defineBarLine "|-basic" #'("||" "" "||")

caesura = { \once \override BreathingSign.text = \markup { \musicglyph #"scripts.caesura.straight" } \breathe }
% caesura_raise = { \once \override BreathingSign.text = \markup {\musicglyph #"scripts.caesura.straight" }
% 		  \once \override BreathingSign.Y-offset = #3
% 		  \breathe }

raiseBracketOpen = {
  \once \override Staff.BarLine.bar-extent = #'( -1.5 . 1.5 )
  \bar "["
}

raiseBracketClose = {
 \once \override Staff.BarLine.bar-extent = #'( -1.5 . 1.5 )
  \bar "]"
}

arrowSpan = {
  \override Staff.TextSpanner.outside-staff-priority = #425
  \override Staff.TextSpanner.bound-padding = #1.0
  \override Staff.TextSpanner.style = #'line
  % \override Staff.TextSpanner.bound-details.right.arrow = ##t
  \override TextSpanner.bound-details.right.text = \markup {
    \column {
      \scale #'( 1 . 1)
      \arrow #"long" ##f #X #UP #1 #0.0
    }
  }
  \override Glissando.arrow-length = #0.5
  \override Glissando.arrow-width = #0.25
  \alterBroken transparent #'(#f #t) TextSpanner % must be a list % change this to fix the invisible arrows (make function)
}

arrowSpanRevert = {
  \revert Staff.TextSpanner.outside-staff-priority
  \revert TextSpanner.bound-padding
  \revert TextSpanner.style
  \revert TextSpanner.bound-details.right.arrow
  \revert Glissando.arrow-length
  \revert Glissando.arrow-width
}

arrowStart = {
  \stopStaff
  \override Staff.Clef.transparent = ##f
  \override Staff.Clef.break-visibility = #all-invisible % this is what messes with the clefs
  \override Staff.StaffSymbol.transparent = ##t
  \startStaff
  \override TextSpanner.thickness = #2
  \override TextSpanner.extra-offset = #'(0 . -3.6)
  % \override TextSpanner.extra-offset = #'(0 . -3.1)
  \override TextSpanner.break-visibility = #all-invisible
  % \override TextSpanner.break-visibility = #all-visible
}


arrowStart_no_comma = {
  \stopStaff
  \override Staff.Clef.transparent = ##f
  \override Staff.Clef.break-visibility = #all-invisible % this is what messes with the clefs
  \override Staff.StaffSymbol.transparent = ##t
  \startStaff
  \override TextSpanner.thickness = #2
  % \override TextSpanner.extra-offset = #'(0 . -3.6)
  \override TextSpanner.extra-offset = #'(0 . -3.1)
  \override TextSpanner.break-visibility = #all-invisible
  % \override TextSpanner.break-visibility = #all-visible
}


arrowInvs = {
  \stopStaff
  \override Staff.BarLine.transparent = ##t
  \startStaff
  }

arrowStop = {
  \stopStaff
  \revert Staff.Clef.transparent
  \revert Staff.Clef.break-visibility
  \revert Staff.StaffSymbol.transparent
  \startStaff
  \revert TextSpanner.thickness
  \revert TextSpanner.extra-offset
  \revert Staff.BarLine.transparent
  \revert TextSpanner.break-visibility
}

cell =
#(define-music-function
  (parser location mus)
  (ly:music?)
  #{
  % \bar "["
  #mus
  % \bar "]"
  #}
)

cellSplit =
#(define-music-function
  (parser location mus)
  (ly:music?)
  #{
				% \bar "||-cell"
  \bar "||"
  #mus
  % \bar "]"
  #}
)

beginArrow =
#(define-music-function
  (parser location mus)
  (ly:music?)
  #{
  \arrowStart
  \arrowSpan
  #mus
  \arrowInvs
  #}
)

beginArrow_no_comma =
#(define-music-function
  (parser location mus)
  (ly:music?)
  #{
  \arrowStart_no_comma
  \arrowSpan
  #mus
  \arrowInvs
  #}
)

lv = \laissezVibrer
trem_duo = \once \override StemTremolo.X-offset = #-0.5
sp = \markup {
  \override \ital
  {"s.p."}
}
sp_paren = \markup {
  \override \ital
  {"(s.p.)"}
}
norm = \markup {
  \override \ital
  {"norm."}
}
norm_paren = \markup {
  \override \ital
  {"(norm.)"}
}
norm_espr = \markup {
 \override \ital
  {"norm. espr."}
}
norm_espr_col = \markup {
 \override \ital
 { \column { "norm."
	     { \vspace #-0.4 "espr." }
	   }
 }
}
norm_espr_paren = \markup {
 \override \ital
  {"(norm. espr.)"}
}

jete = \markup{ \override \ital "jeté" }

#(define (test-stencil grob text)
   (let* ((orig (ly:grob-original grob))
          (siblings (ly:spanner-broken-into orig)) ; have we been split?
          (refp (ly:grob-system grob))
          (left-bound (ly:spanner-bound grob LEFT))
          (right-bound (ly:spanner-bound grob RIGHT))
          (elts-L (ly:grob-array->list (ly:grob-object left-bound 'elements)))
          (elts-R (ly:grob-array->list (ly:grob-object right-bound 'elements)))
          (break-alignment-L
           (filter
            (lambda (elt) (grob::has-interface elt 'break-alignment-interface))
            elts-L))
          (break-alignment-R
           (filter
            (lambda (elt) (grob::has-interface elt 'break-alignment-interface))
            elts-R))
          (break-alignment-L-ext (ly:grob-extent (car break-alignment-L) refp X))
          (break-alignment-R-ext (ly:grob-extent (car break-alignment-R) refp X))
          (num
           (markup text))
          (num
           (if (or (null? siblings)
                   (eq? grob (car siblings)))
               num
               (make-parenthesize-markup num)))
          (num (grob-interpret-markup grob num))
          (num-stil-ext-X (ly:stencil-extent num X))
          (num-stil-ext-Y (ly:stencil-extent num Y))
          (num (ly:stencil-aligned-to num X CENTER))
          (num
           (ly:stencil-translate-axis
            num
            (+ (interval-length break-alignment-L-ext)
              (* 0.5
                (- (car break-alignment-R-ext)
                  (cdr break-alignment-L-ext))))
            X))
          (bracket-L
           (markup
            #:path
            0.1 ; line-thickness
            `((moveto 0.5 ,(* 0.5 (interval-length num-stil-ext-Y)))
              (lineto ,(* 0.5
                         (- (car break-alignment-R-ext)
                           (cdr break-alignment-L-ext)
                           (interval-length num-stil-ext-X)))
                ,(* 0.5 (interval-length num-stil-ext-Y)))
              (closepath)
              (rlineto 0.0
                ,(if (or (null? siblings) (eq? grob (car siblings)))
                     -1.0 0.0)))))
          (bracket-R
           (markup
            #:path
            0.1
            `((moveto ,(* 0.5
                         (- (car break-alignment-R-ext)
                           (cdr break-alignment-L-ext)
                           (interval-length num-stil-ext-X)))
                ,(* 0.5 (interval-length num-stil-ext-Y)))
              (lineto 0.5
                ,(* 0.5 (interval-length num-stil-ext-Y)))
              (closepath)
              (rlineto 0.0
                ,(if (or (null? siblings) (eq? grob (last siblings)))
                     -1.0 0.0)))))
          (bracket-L (grob-interpret-markup grob bracket-L))
          (bracket-R (grob-interpret-markup grob bracket-R))
          (num (ly:stencil-combine-at-edge num X LEFT bracket-L 0.4))
          (num (ly:stencil-combine-at-edge num X RIGHT bracket-R 0.4)))
     num))


#(define-public (Measure_attached_spanner_engraver context)
   (let ((span '())
         (finished '())
         (event-start '())
         (event-stop '()))
     (make-engraver
      (listeners ((measure-counter-event engraver event)
                  (if (= START (ly:event-property event 'span-direction))
                      (set! event-start event)
                      (set! event-stop event))))
      ((process-music trans)
       (if (ly:stream-event? event-stop)
           (if (null? span)
               (ly:warning "You're trying to end a measure-attached spanner but you haven't started one.")
               (begin (set! finished span)
                 (ly:engraver-announce-end-grob trans finished event-start)
                 (set! span '())
                 (set! event-stop '()))))
       (if (ly:stream-event? event-start)
           (begin (set! span (ly:engraver-make-grob trans 'MeasureCounter event-start))
             (set! event-start '()))))
      ((stop-translation-timestep trans)
       (if (and (ly:spanner? span)
                (null? (ly:spanner-bound span LEFT))
                (moment<=? (ly:context-property context 'measurePosition) ZERO-MOMENT))
           (ly:spanner-set-bound! span LEFT
             (ly:context-property context 'currentCommandColumn)))
       (if (and (ly:spanner? finished)
                (moment<=? (ly:context-property context 'measurePosition) ZERO-MOMENT))
           (begin
            (if (null? (ly:spanner-bound finished RIGHT))
                (ly:spanner-set-bound! finished RIGHT
                  (ly:context-property context 'currentCommandColumn)))
            (set! finished '())
            (set! event-start '())
            (set! event-stop '()))))
      ((finalize trans)
       (if (ly:spanner? finished)
           (begin
            (if (null? (ly:spanner-bound finished RIGHT))
                (set! (ly:spanner-bound finished RIGHT)
                      (ly:context-property context 'currentCommandColumn)))
            (set! finished '())))
       (if (ly:spanner? span)
           (begin
            (ly:warning "I think there's a dangling measure-attached spanner :-(")
            (ly:grob-suicide! span)
            (set! span '())))))))

% function itself
repeatBracket = #(define-music-function
     (parser location N note)
     (number? ly:music?)
      #{
        \override Staff.MeasureCounter.stencil =
        #(lambda (grob) (test-stencil grob #{ #(string-append(number->string N) "x") #} ))
        \startMeasureCount
        \repeat volta #N { $note }
        \stopMeasureCount
      #}
			)

timeBracket = #(define-music-function
     (parser location M note)
     (markup? ly:music?)
      #{
        \override Staff.MeasureCounter.stencil =
		#(lambda (grob) (test-stencil grob #{ #(string-append M "\"") #} ))
        \startMeasureCount
        $note
        \stopMeasureCount
      #}
     )


%%%%%%%%%%%%%%%%%%%%%%%shitty boxes %%%%%%%%%%%%%%%%%%%%%%%%%%

headWhole   = {
  \once \override NoteHead  #'stencil = #ly:text-interface::print
  \once \override NoteHead #'text = #(markup
  #:musicglyph "noteheads.s0" )
  \once \override Stem.length = #0
}

square-one =
\once \override Stem.stencil = #(lambda (grob)
    (let* ((note (ly:stem::print grob))
           ; (chord (ly:
           (combo-stencil (ly:stencil-add
               note
               (box-stencil note 0.1 3))))
          (ly:make-stencil (ly:stencil-expr combo-stencil)
            (ly:stencil-extent note X)
            (ly:stencil-extent note Y)
            )))

%%%%%%%%%%%%%%%%%%%%%%%%%% Box Defn %%%%%%%%%%%%%%%%%%%%%%%%%%

% #(define-event-class 'music-boxer-event 'span-event)

% #(define-event-class 'box-event 'music-event)

% #(define (add-grob-definition grob-name grob-entry)
%    (let* ((meta-entry   (assoc-get 'meta grob-entry))
%           (class        (assoc-get 'class meta-entry))
%           (ifaces-entry (assoc-get 'interfaces meta-entry)))
%      ;; change ly:grob-properties? to list? to work from 2.19.12 back to at least 2.18.2
%      (set-object-property! grob-name 'translation-type? ly:grob-properties?)
%      (set-object-property! grob-name 'is-grob? #t)
%      (set! ifaces-entry (append (case class
%                                   ((Item) '(item-interface))
%                                   ((Spanner) '(spanner-interface))
%                                   ((Paper_column) '((item-interface
%                                                      paper-column-interface)))
%                                   ((System) '((system-interface
%                                                spanner-interface)))
%                                   (else '(unknown-interface)))
%                           ifaces-entry))
%      (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
%      (set! ifaces-entry (cons 'grob-interface ifaces-entry))
%      (set! meta-entry (assoc-set! meta-entry 'name grob-name))
%      (set! meta-entry (assoc-set! meta-entry 'interfaces
%                         ifaces-entry))
%      (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
%      (set! all-grob-descriptions
%            (cons (cons grob-name grob-entry)
%              all-grob-descriptions))))

% #(define (define-grob-property symbol type? description)
%    (if (not (equal? (object-property symbol 'backend-doc) #f))
%        (ly:error (_ "symbol ~S redefined") symbol))

%    (set-object-property! symbol 'backend-type? type?)
%    (set-object-property! symbol 'backend-doc description)
%    symbol)

% #(map
%   (lambda (x)
%     (apply define-grob-property x))

%   `(
%      (fill-color ,color? "Background color for filling the rectangle")
%      (acknowledge-finger-interface ,boolean? "Include fingerings in box?")
%      (acknowledge-script-interface ,boolean? "Include scripts in box?")
%      (zigzag-left  ,number? "Zigzag size for left edge")
%      (zigzag-right ,number? "Zigzag size for right edge")
%      ; add more properties here
%      ))

% #(define (makeDeltaSpan
%           y-l-lower y-l-upper         ; number: Y-dimensions (left edge)
%           y-r-lower y-r-upper         ; number: Y-dimensions (left edge)
%           frame-color fill-color      ; (color of ##f): colors for outer and inner polygon (won't be drawn if set to ##f)
%           stepLeft stepRight          ; number: size of zigzag elements for left and right edge (vertical edge / no zigzag if set to zero)
%           open-on-bottom open-on-top  ; boolean: no visible frame on bottom/top edge (no distance between inner and outer polygon's edges)
%           thick                       ; number: frame thickness - distance between inner and outer polygon's edges
%           pad                         ; number: broken-bound-padding - amount to shorten spanners where separated by a line break
%           X-ext-param                 ; pair: the spanner's X-dimensions
%           open-on-left open-on-right  ; boolean: no visible frame on left/right edge (no distance between inner and outer polygon's edges)
%           ;   We'll assume that this indicates a line break!
%           radius                      ; number: radius for "round-filled-polygon" procedure
%           )

%    (let* (
%            (h-thick (* thick (sqrt 2)))  ; X-distance between left and right edges of inner and outer polygon. Must be "thick" * sqrt 2  (Pythagoras)
%            (l-width (* stepLeft  0.5))   ; X-distance of zigzag corners
%            (r-width (* stepRight 0.5))
%            (Y-ext (cons 0 0))            ; dummy, needed for ly:stencil-expr  (is there a way without it?)
%            (X-ext (cons
%                    (if (> stepLeft 0)    ; left edge has zigzag shape
%                        (- (+ (car X-ext-param) (/ l-width 2)) h-thick)  ; Half of the zigzag space will be taken from inside, other half from the outside. Frame space taken from outside.
%                        (if open-on-left  (- (car X-ext-param) h-thick) (- (car X-ext-param) thick))
%                        )
%                    (if (> stepRight 0)   ; right edge has zigzag shape
%                        (+ (- (cdr X-ext-param) (/ r-width 2)) h-thick)
%                        (if open-on-right (+ (cdr X-ext-param) h-thick) (+ (cdr X-ext-param) thick))
%                        )))
%            (X-ext (cons
%                    (if open-on-left  (- (+ (car X-ext) pad) (/ l-width 2)) (car X-ext))     ; shorten/lengthen by broken-bound-padding if spanner is broken
%                    (if open-on-right (+ (- (cdr X-ext) pad) (/ r-width 2)) (cdr X-ext))))
%            (points (list))       ; will contain coordinates for outer polygon
%            (points-i (list))     ; will contain coordinates for inner polygon
%            (slope-upper (/ (- y-r-upper y-l-upper) (- (cdr X-ext) (car X-ext))))  ; slope of the polygon's upper edge
%            (slope-lower (/ (- y-r-lower y-l-lower) (- (cdr X-ext) (car X-ext))))  ; slope of the polygon's lower edge
%            (d-upper (if open-on-top    0  (* thick (sqrt (+ (expt slope-upper 2) 1)))))  ; (Pythagoras)
%            ; Y-distance between upper edges of inner and outer polygon. Equal to "thick" if upper edge is horizontal.
%            ; Increases as the upper edge's slope increases.
%            (d-lower (if open-on-bottom 0  (* thick (sqrt (+ (expt slope-lower 2) 1)))))  ; same for lower edge
%            ; stuff for later calculations:
%            (xtemp 0)
%            (yLowerLimit 0)
%            (yUpperLimit 0)
%            (xp 0)
%            (yp 0)
%            (jumps 0)
%            )

%      ; calculate outer polygon's borders:

%      ; lower-left corner:
%      (set! points (list (cons (car X-ext) y-l-lower)))

%      ; calculate coordinates for left (outer) zigzag border:
%      (if (and (> stepLeft 0) (not open-on-left))
%          (let loop ((cnt y-l-lower))
%            (if (< cnt y-l-upper)
%                (begin
%                 (if (and (< cnt y-l-upper) (> cnt y-l-lower))  ; only add to list if point is inside the given Y-range
%                     (set! points (cons (cons    (car X-ext)             cnt                 ) points)))
%                 (if (and (< (+ cnt (/ stepLeft 2)) y-l-upper) (> (+ cnt (/ stepLeft 2)) y-l-lower))
%                     (set! points (cons (cons (- (car X-ext) l-width) (+ cnt (/ stepLeft 2)) ) points)))
%                 (loop (+ cnt stepLeft))))))

%      ; upper-left corner:
%      (set! points (cons
%                    (cons (car X-ext) y-l-upper)
%                    points ))
%      ; upper-right corner:
%      (set! points (cons
%                    (cons (cdr X-ext) y-r-upper)
%                    points ))
%      ; right outer zigzag border:
%      (if (and (> stepRight 0) (not open-on-right))
%          (let loop ((cnt y-r-upper))
%            (if (> cnt y-r-lower)
%                (begin
%                 (if (and (< cnt y-r-upper) (> cnt y-r-lower))
%                     (set! points (cons (cons    (cdr X-ext)             cnt                  ) points)))
%                 (if (and (< (- cnt (/ stepRight 2)) y-r-upper) (> (- cnt (/ stepRight 2)) y-r-lower))
%                     (set! points (cons (cons (+ (cdr X-ext) r-width) (- cnt (/ stepRight 2)) ) points)))
%                 (loop (- cnt stepRight))))))

%      ; lower-right corner:
%      (set! points (cons
%                    (cons (cdr X-ext) y-r-lower)
%                    points ))

%      ; shrink X-ext for use with inner stuff:
%      (if (not open-on-left)
%          (if (> stepLeft 0)
%              (set! X-ext (cons (+ (car X-ext) h-thick) (cdr X-ext)))
%              (set! X-ext (cons (+ (car X-ext)   thick) (cdr X-ext)))
%              )
%          )
%      (if (not open-on-right)
%          (if (> stepRight 0)
%              (set! X-ext (cons (car X-ext) (- (cdr X-ext) h-thick)))
%              (set! X-ext (cons (car X-ext) (- (cdr X-ext)   thick)))
%              )
%          ) ; Now X-ext represents INNER polygon's width WITHOUT the zigzag corners

%      ; Now calculate inner borders:
%      ; xp and yp will be the coordinates of the corner currently being calculated

%      ; calculate lower-left corner:

%      (set! yLowerLimit y-l-lower)
%      (set! yUpperLimit y-l-upper)

%      (if open-on-left
%          (begin
%           (set! xp (car X-ext))
%           (set! yp (+ y-l-lower d-lower))
%           )
%          (if (> stepLeft 0)
%              (if (not (eq? slope-lower -1))
%                  (begin
%                   (set! jumps 0)
%                   (while (> (- (+ (* slope-lower h-thick) d-lower) (* jumps stepLeft)) stepLeft)
%                     (set! jumps (+ 1 jumps)))
%                   (set! xtemp (/ (- (+ h-thick (* jumps stepLeft)) d-lower) (+ slope-lower 1)))
%                   ; results from the solution for a system of two equations. Forgive me, I'm a maths teacher :-)
%                   (if (< xtemp (- h-thick (/ stepLeft 2)))
%                       (if (= 1 slope-lower)
%                           (set! xtemp h-thick)
%                           (set! xtemp
%                                 (/ (+ (- d-lower (* stepLeft (+ 1 jumps))) h-thick) (- 1 slope-lower)))))  ; another system of 2 equations...
%                   (set! xp (+ (- (car X-ext) h-thick) xtemp))
%                   (set! yp (+ (+ y-l-lower (* slope-lower xtemp)) d-lower))
%                   )
%                  )
%              (begin
%               (set! xp (car X-ext))
%               (set! yp (+ (+ y-l-lower (* thick slope-lower)) d-lower))
%               )
%              )
%          )

%      ; insert lower-left corner's coordinates into list:
%      (if (not (and (and (not open-on-left) (> stepLeft 0)) (eq? slope-lower -1)))
%          (begin
%           (set! points-i (cons (cons xp yp) points-i))
%           (set! yLowerLimit yp)
%           )
%          )

%      ; calculate upper-left corner:
%      (if open-on-left
%          (begin
%           (set! xp (car X-ext))
%           (set! yp (- y-l-upper d-upper))
%           )
%          (if (> stepLeft 0)
%              (if (not (eq? slope-upper 1))
%                  (begin
%                   (set! jumps 0)
%                   (while (<
%                           (+ (- (* slope-upper h-thick) d-upper) (* jumps stepLeft))
%                           (- stepLeft))
%                     (set! jumps (+ jumps 1)))
%                   (set! xtemp (/ (- d-upper (+ h-thick (* jumps stepLeft))) (- slope-upper 1)))
%                   (if (< xtemp (- h-thick (/ stepLeft 2)))
%                       (if (= -1 slope-upper)
%                           (set! xtemp h-thick)
%                           (set! xtemp
%                                 (/ (- (- (* stepLeft (+ 1 jumps)) d-upper) h-thick) (- (- 1) slope-upper)))
%                           )
%                       )
%                   (set! xp (+ (- (car X-ext) h-thick) xtemp))
%                   (set! yp (- (+ y-l-upper (* slope-upper xtemp)) d-upper))
%                   )
%                  )
%              (begin
%               (set! xp (car X-ext))
%               (set! yp (- (+ y-l-upper (* thick slope-upper)) d-upper))
%               )
%              )
%          )

%      (if (not
%           (and (and (not open-on-left) (> stepLeft 0)) (eq? slope-upper 1))
%           )
%          (set! yUpperLimit yp))


%      ; left (inner) zigzag:
%      (if (and (> stepLeft 0) (not open-on-left))
%          (begin
%           (let loop ((cnt y-l-lower))
%             (if (< cnt y-l-upper)
%                 (begin
%                  (if (and (> cnt yLowerLimit) (< cnt yUpperLimit))
%                      (set! points-i (cons (cons    (car X-ext)             cnt                 ) points-i))
%                      )
%                  (if (and (> (+ cnt (/ stepLeft 2)) yLowerLimit) (< (+ cnt (/ stepLeft 2)) yUpperLimit))
%                      (set! points-i (cons (cons (- (car X-ext) l-width) (+ cnt (/ stepLeft 2)) ) points-i))
%                      )
%                  (loop (+ cnt stepLeft))
%                  )
%                 )
%             )
%           )
%          )

%      ; insert upper-left corner (yes, AFTER the zigzag points, so all the points will be given in clockwise order):
%      (if (not
%           (and (and (not open-on-left) (> stepLeft 0)) (eq? slope-upper 1))
%           )
%          (set! points-i (cons (cons xp yp) points-i)))

%      ; calculate upper-right corner:

%      (set! yLowerLimit y-r-lower)
%      (set! yUpperLimit y-r-upper)

%      (if open-on-right
%          (begin
%           (set! xp (cdr X-ext))
%           (set! yp (- y-r-upper d-upper))
%           )
%          (if (> stepRight 0)
%              (if (not (eq? slope-upper -1))
%                  (begin
%                   (set! jumps 0)
%                   (while (<
%                           (+ (- (* slope-upper (- h-thick)) d-upper) (* jumps stepRight))
%                           (- stepRight))
%                     (set! jumps (+ jumps 1)))
%                   (set! xtemp (/ (- d-upper (+ h-thick (* jumps stepRight))) (+ slope-upper 1)))
%                   (if (> xtemp (- (/ stepRight 2) h-thick  ))
%                       (if (= 1 slope-upper)
%                           (set! xtemp (- h-thick))
%                           (set! xtemp
%                                 (/ (- (- (* stepRight (+ 1 jumps)) d-upper) h-thick) (- 1 slope-upper)))
%                           )
%                       )
%                   (set! xp (+ (+ (cdr X-ext) h-thick) xtemp))
%                   (set! yp (- (+ y-r-upper (* slope-upper xtemp)) d-upper))
%                   )
%                  )
%              (begin
%               (set! xp (cdr X-ext))
%               (set! yp (- (- y-r-upper (* thick slope-upper)) d-upper))
%               )
%              )
%          )

%      ; insert upper-right corner:
%      (if (not
%           (and (and (not open-on-right) (> stepRight 0)) (eq? slope-upper -1)))
%          (begin
%           (set! points-i (cons (cons xp yp) points-i))
%           (set! yUpperLimit yp)))

%      ; calculate lower-right corner:
%      (if open-on-right
%          (begin
%           (set! xp (cdr X-ext))
%           (set! yp (+ y-r-lower d-lower))
%           )
%          (if (> stepRight 0)
%              (if (not (eq? slope-lower 1))
%                  (begin
%                   (set! jumps 0)
%                   (while (> (- (- d-lower (* slope-lower h-thick)) (* jumps stepRight)) stepRight)
%                     (set! jumps (+ 1 jumps)))
%                   (set! xtemp (/ (- (+ h-thick (* jumps stepRight)) d-lower) (- slope-lower 1)))
%                   (if (> xtemp (- (/ stepRight 2) h-thick)   )
%                       (if (= -1 slope-lower)
%                           (set! xtemp (- h-thick))
%                           (set! xtemp
%                                 (/ (+ (- d-lower (* stepRight (+ 1 jumps))) h-thick) (- -1 slope-lower)))))
%                   (set! xp (+ (+ (cdr X-ext) h-thick) xtemp))
%                   (set! yp (+ (+ y-r-lower (* slope-lower xtemp)) d-lower))
%                   )
%                  )
%              (begin
%               (set! xp (cdr X-ext))
%               (set! yp (+ (- y-r-lower (* thick slope-lower)) d-lower))
%               )
%              )
%          )

%      (if (not (and (and (not open-on-right) (> stepRight 0)) (eq? slope-lower 1)))
%          (set! yLowerLimit yp))

%      ; right zigzag:
%      (if (and (> stepRight 0) (not open-on-right))
%          (begin
%           (let loop ((cnt y-r-upper))
%             (if (> cnt y-r-lower)
%                 (begin
%                  (if (and (> cnt yLowerLimit) (< cnt yUpperLimit))
%                      (set! points-i (cons (cons    (cdr X-ext)             cnt                  ) points-i)))
%                  (if (and (> (- cnt (/ stepRight 2)) yLowerLimit) (< (- cnt (/ stepRight 2)) yUpperLimit))
%                      (set! points-i (cons (cons (+ (cdr X-ext) r-width) (- cnt (/ stepRight 2)) ) points-i)))
%                  (loop (- cnt stepRight))
%                  )
%                 )
%             )
%           )
%          )

%      ; insert lower-right corner:
%      (if (not (and (and (not open-on-right) (> stepRight 0)) (eq? slope-lower 1)))
%          (set! points-i (cons (cons xp yp) points-i)))

%      (ly:stencil-add
%       ; draw outer polygon:
%       (if (color? frame-color)  ; only add stencil if set to a valid color (could also be set to ##f)
%           (ly:make-stencil (list 'color frame-color
%                              (ly:stencil-expr (ly:round-filled-polygon points radius))
%                              X-ext Y-ext))
%           empty-stencil)
%       ; draw inner polygon:
%       (if (color? fill-color)   ; only add stencil if set to a valid color (could also be set to ##f)
%           (ly:make-stencil (list 'color fill-color
%                              (ly:stencil-expr (ly:round-filled-polygon points-i radius))
%                              X-ext Y-ext))
%           empty-stencil)
%       )
%      )
%    )

% #(define (music-boxer-stencil grob)
%    (let* ((elts (ly:grob-object grob 'elements))
%           (refp-X (ly:grob-common-refpoint-of-array grob elts X))
%           (X-ext (ly:relative-group-extent elts refp-X X))
%           (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
%           (Y-ext (ly:relative-group-extent elts refp-Y Y))
%           (padding (ly:grob-property grob 'padding 0.3))
%           (slope (ly:grob-property grob 'slope 0))             ; Y-difference between left and right edge (artificially applied)
%           (extra-dy (ly:grob-property grob 'extra-dy 0))       ; additional box height
%           (bb-padding (ly:grob-property grob 'broken-bound-padding -6))
%           (thick (ly:grob-property grob 'thickness 0.1))
%           (X-ext (interval-widen X-ext padding))               ; already applied here because makeDeltaSpan has no padding parameter
%           (Y-ext (interval-widen Y-ext padding))               ; dto.
%           (Y-ext (interval-widen Y-ext thick))                 ; because makeDeltaSpan will take the Y-space for frame thickness from inside
%           (Y-ext (interval-widen Y-ext (/ extra-dy 2)))
%           (frame-color (ly:grob-property grob 'color black))
%           (fill-color (ly:grob-property grob 'fill-color white))
%           (offset (ly:grob-relative-coordinate grob refp-X X))
%           (stepLeft  (ly:grob-property grob 'zigzag-left  0))  ; zigzag size for left  edge (only used as approximate value )
%           (stepRight (ly:grob-property grob 'zigzag-right 0))  ; dto., right edge
%           (open-on-left
%            (and (ly:spanner? grob)
%                 (= 1 (ly:item-break-dir (ly:spanner-bound grob LEFT)))))
%           (open-on-right
%            (and (ly:spanner? grob)
%                 (= -1 (ly:item-break-dir (ly:spanner-bound grob RIGHT)))))

%           (y-l-lower (- (car Y-ext) (/ slope 2)))
%           (y-l-upper (- (cdr Y-ext) (/ slope 2)))
%           (y-r-lower (+ (car Y-ext) (/ slope 2)))
%           (y-r-upper (+ (cdr Y-ext) (/ slope 2)))
%           (cnt 0)  ; counter, will be used later...
%           (stil empty-stencil))
%      (if (not (= stepLeft 0))
%          (begin   ; calculate exact size for only entire zigzag squiggles should be used
%            (set! cnt (round (/ (- y-l-upper y-l-lower) stepLeft)))
%            (if (> cnt 0)
%                (set! stepLeft (/ (- y-l-upper y-l-lower) cnt))
%                (set! stepLeft 0))))
%      (if (not (= stepRight 0))
%          (begin
%           (set! cnt (round (/ (- y-r-upper y-r-lower) stepRight)))
%           (if (> cnt 0)
%               (set! stepRight (/ (- y-r-upper y-r-lower) cnt))
%               (set! stepRight 0))))
%      (set! stil
%            (makeDeltaSpan
%             y-l-lower y-l-upper
%             y-r-lower y-r-upper
%             frame-color fill-color
%             stepLeft stepRight
%             #f #f                     ; open-on-bottom open-on-top
%             thick bb-padding X-ext open-on-left open-on-right
%             0                         ; radius
%             )
%            )
%      (ly:stencil-translate-axis stil (- offset) X)
%      )
%    )

% %#(define box-stil music-boxer-stencil)

% %% Test callback for Box.stencil
% #(define (box-stil grob)
%    (let* ((elts (ly:grob-object grob 'elements))
%           (refp-X (ly:grob-common-refpoint-of-array grob elts X))
%           (X-ext (ly:relative-group-extent elts refp-X X))
%           (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))

%           ; following line triggers vertical spacing too early
%           ;(Y-ext (ly:relative-group-extent elts refp-Y Y))
%           (Y-ext '(-3 . 3))
%           (padding (ly:grob-property grob 'padding 0.3))
%           (slope (ly:grob-property grob 'slope 0))             ; Y-difference between left and right edge (artificially applied)
%           (extra-dy (ly:grob-property grob 'extra-dy 0))       ; additional box height
%           (bb-padding (ly:grob-property grob 'broken-bound-padding -6))
%           (thick (ly:grob-property grob 'thickness 0.1))
%           (X-ext (interval-widen X-ext padding))               ; already applied here because makeDeltaSpan has no padding parameter
%           (Y-ext (interval-widen Y-ext padding))               ; dto.
%           (Y-ext (interval-widen Y-ext thick))                 ; because makeDeltaSpan will take the Y-space for frame thickness from inside
%           (Y-ext (interval-widen Y-ext (/ extra-dy 2)))
%           (frame-color (ly:grob-property grob 'color black))
%           (fill-color (ly:grob-property grob 'fill-color white))
%           (offset (ly:grob-relative-coordinate grob refp-X X))
%           (stepLeft  (ly:grob-property grob 'zigzag-left  0))  ; zigzag size for left  edge (only used as approximate value )
%           (stepRight (ly:grob-property grob 'zigzag-right 0))  ; dto., right edge
%           (open-on-left
%            (and (ly:spanner? grob)
%                 (= 1 (ly:item-break-dir (ly:spanner-bound grob LEFT)))))
%           (open-on-right
%            (and (ly:spanner? grob)
%                 (= -1 (ly:item-break-dir (ly:spanner-bound grob RIGHT)))))

%           (y-l-lower (- (car Y-ext) (/ slope 2)))
%           (y-l-upper (- (cdr Y-ext) (/ slope 2)))
%           (y-r-lower (+ (car Y-ext) (/ slope 2)))
%           (y-r-upper (+ (cdr Y-ext) (/ slope 2)))
%           (cnt 0)  ; counter, will be used later...
%           (stil empty-stencil))
%      (if (not (= stepLeft 0))
%          (begin   ; calculate exact size for only entire zigzag squiggles should be used
%            (set! cnt (round (/ (- y-l-upper y-l-lower) stepLeft)))
%            (if (> cnt 0)
%                (set! stepLeft (/ (- y-l-upper y-l-lower) cnt))
%                (set! stepLeft 0))))
%      (if (not (= stepRight 0))
%          (begin
%           (set! cnt (round (/ (- y-r-upper y-r-lower) stepRight)))
%           (if (> cnt 0)
%               (set! stepRight (/ (- y-r-upper y-r-lower) cnt))
%               (set! stepRight 0))))
%      (set! stil
%            (makeDeltaSpan
%             y-l-lower y-l-upper
%             y-r-lower y-r-upper
%             frame-color fill-color
%             stepLeft stepRight
%             #f #f                     ; open-on-bottom open-on-top
%             thick bb-padding X-ext open-on-left open-on-right
%             0                         ; radius
%             )
%            )
%      (ly:stencil-translate-axis stil (- offset) X)
%      )
%    )

% #(add-grob-definition
%   'Box
%   `(
%      (direction . ,UP)
%      (outside-staff-priority . 251) ; just larger than DynamicText
%      (stencil . ,box-stil)
%      (meta . ((class . Item)
%               (interfaces . (outside-staff-interface))))))

% #(add-grob-definition
%   'MusicBoxer
%   `(
%      (direction . ,UP)
%      (outside-staff-priority . 251) ; just larger than DynamicText
%      (stencil . ,music-boxer-stencil)
%      (meta . ((class . Spanner)
%               (interfaces . (outside-staff-interface))))))


% #(define box-types
%    '(
%       (BoxEvent
%        . ((description . "A box encompassing music at a single timestep.")
%           (types . (general-music box-event music-event event))
%           ))
%       ))

% #(define music-boxer-types
%    '(
%       (MusicBoxerEvent
%        . ((description . "Used to signal where boxes encompassing music start and stop.")
%           (types . (general-music music-boxer-event span-event event))
%           ))
%       ))


% #(set!
%   music-boxer-types
%   (map (lambda (x)
%          (set-object-property! (car x)
%            'music-description
%            (cdr (assq 'description (cdr x))))
%          (let ((lst (cdr x)))
%            (set! lst (assoc-set! lst 'name (car x)))
%            (set! lst (assq-remove! lst 'description))
%            (hashq-set! music-name-to-property-table (car x) lst)
%            (cons (car x) lst)))
%     music-boxer-types))

% #(set!
%   box-types
%   (map (lambda (x)
%          (set-object-property! (car x)
%            'music-description
%            (cdr (assq 'description (cdr x))))
%          (let ((lst (cdr x)))
%            (set! lst (assoc-set! lst 'name (car x)))
%            (set! lst (assq-remove! lst 'description))
%            (hashq-set! music-name-to-property-table (car x) lst)
%            (cons (car x) lst)))
%     box-types))

% #(set! music-descriptions
%        (append music-boxer-types music-descriptions))

% #(set! music-descriptions
%        (append box-types music-descriptions))

% #(set! music-descriptions
%        (sort music-descriptions alist<?))


% #(define (add-bound-item spanner item)
%    (if (null? (ly:spanner-bound spanner LEFT))
%        (ly:spanner-set-bound! spanner LEFT item)
%        (ly:spanner-set-bound! spanner RIGHT item)))

% musicBoxerEngraver =
% #(lambda (context)
%    (let ((span '())
%          (finished '())
%          (current-event '())
%          (event-start '())
%          (event-stop '())
%          )

%      `((listeners
%         (music-boxer-event .
%           ,(lambda (engraver event)
%              (if (= START (ly:event-property event 'span-direction))
%                  (set! event-start event)
%                  (set! event-stop event)))))

%        (acknowledgers
%         (note-column-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:spanner? span)
%                  (begin
%                   (ly:pointer-group-interface::add-grob span 'elements grob)
%                   (add-bound-item span grob)))
%              (if (ly:spanner? finished)
%                  (begin
%                   (ly:pointer-group-interface::add-grob finished 'elements grob)
%                   (add-bound-item finished grob)))))

%         (inline-accidental-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:spanner? span)
%                  (begin
%                   (ly:pointer-group-interface::add-grob span 'elements grob)))
%              (if (ly:spanner? finished)
%                  (ly:pointer-group-interface::add-grob finished 'elements grob))))

%         (dots-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:spanner? span)
%                  (begin
%                   (ly:pointer-group-interface::add-grob span 'elements grob)))
%              (if (ly:spanner? finished)
%                  (ly:pointer-group-interface::add-grob finished 'elements grob))))

%         (ledger-line-spanner-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:spanner? span)
%                  (begin
%                   (ly:pointer-group-interface::add-grob span 'elements grob)))
%              (if (ly:spanner? finished)
%                  (ly:pointer-group-interface::add-grob finished 'elements grob))))

%         (script-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (and (ly:spanner? span)
%                       (eq? #t (ly:grob-property span 'acknowledge-script-interface)))
%                  (begin
%                   (ly:pointer-group-interface::add-grob span 'elements grob)))
%              (if (and (ly:spanner? finished)
%                       (eq? #t (ly:grob-property finished 'acknowledge-script-interface)))
%                  (ly:pointer-group-interface::add-grob finished 'elements grob))))

%         (finger-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (and (ly:spanner? span)
%                       (eq? #t (ly:grob-property span 'acknowledge-finger-interface)))
%                  (begin
%                   (ly:pointer-group-interface::add-grob span 'elements grob)))
%              (if (and (ly:spanner? finished)
%                       (eq? #t (ly:grob-property finished 'acknowledge-finger-interface)))
%                  (ly:pointer-group-interface::add-grob finished 'elements grob))))

%         ;; add additional interfaces to acknowledge here

%         )

%        (process-music .
%          ,(lambda (trans)
%             (if (ly:stream-event? event-stop)
%                 (if (null? span)
%                     (ly:warning "No start to this box.")
%                     (begin
%                      (set! finished span)
%                      (ly:engraver-announce-end-grob trans finished event-start)
%                      (set! span '())
%                      (set! event-stop '()))))
%             (if (ly:stream-event? event-start)
%                 (begin
%                  (set! span (ly:engraver-make-grob trans 'MusicBoxer event-start))
%                  (set! event-start '())))))

%        (stop-translation-timestep .
%          ,(lambda (trans)
%             (if (and (ly:spanner? span)
%                      (null? (ly:spanner-bound span LEFT)))
%                 (ly:spanner-set-bound! span LEFT
%                   (ly:context-property context 'currentMusicalColumn)))
%             (if (ly:spanner? finished)
%                 (begin
%                  (if (null? (ly:spanner-bound finished RIGHT))
%                      (ly:spanner-set-bound! finished RIGHT
%                        (ly:context-property context 'currentMusicalColumn)))
%                  (set! finished '())
%                  (set! event-start '())
%                  (set! event-stop '())))))

%        (finalize
%         (lambda (trans)
%           (if (ly:spanner? finished)
%               (begin
%                (if (null? (ly:spanner-bound finished RIGHT))
%                    (set! (ly:spanner-bound finished RIGHT)
%                          (ly:context-property context 'currentMusicalColumn)))
%                (set! finished '())))
%           (if (ly:spanner? span)
%               (begin
%                (ly:warning "unterminated box :-(")
%                (ly:grob-suicide! span)
%                (set! span '())))
%           )))))


% boxEngraver =
% #(lambda (context)
%    (let ((box '())
%          (ev '()))

%      `((listeners
%         (box-event .
%           ,(lambda (engraver event)
%              (set! ev event))))

%        (acknowledgers
%         (note-column-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:grob? box)
%                  (begin
%                   ; (set! (ly:grob-parent box X) grob) ;; ??
%                   (set! (ly:grob-parent box Y) grob)
%                   (ly:pointer-group-interface::add-grob box 'elements grob)))))

%         (inline-accidental-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:item? box)
%                  (ly:pointer-group-interface::add-grob box 'elements grob))))

%         (dots-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:item? box)
%                  (ly:pointer-group-interface::add-grob box 'elements grob))))

%         (ledger-line-spanner-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (ly:item? box)
%                  (ly:pointer-group-interface::add-grob box 'elements grob))))

%         (script-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (and (ly:item? box) (eq? #t (ly:grob-property box 'acknowledge-script-interface)))
%                  (ly:pointer-group-interface::add-grob box 'elements grob))))

%         (finger-interface .
%           ,(lambda (engraver grob source-engraver)
%              (if (and (ly:item? box) (eq? #t (ly:grob-property box 'acknowledge-finger-interface)))
%                  (ly:pointer-group-interface::add-grob box 'elements grob))))

%         ;; add additional interfaces to acknowledge here

%         )

%        (process-music .
%          ,(lambda (trans)
%             (if (ly:stream-event? ev)
%                 (begin
%                  (set! box (ly:engraver-make-grob trans 'Box ev))
%                  (set! ev '())))))
%        (stop-translation-timestep .
%          ,(lambda (trans)
%             (set! box '()))))))

% musicBoxerStart =
% #(make-span-event 'MusicBoxerEvent START)

% musicBoxerEnd =
% #(make-span-event 'MusicBoxerEvent STOP)

% box = #(make-music 'BoxEvent)








#(define-event-class 'music-boxer-event 'span-event)

#(define-event-class 'box-event 'music-event)

#(define (add-grob-definition grob-name grob-entry)
   (let* ((meta-entry   (assoc-get 'meta grob-entry))
          (class        (assoc-get 'class meta-entry))
          (ifaces-entry (assoc-get 'interfaces meta-entry)))
     ;; change ly:grob-properties? to list? to work from 2.19.12 back to at least 2.18.2
     (set-object-property! grob-name 'translation-type? ly:grob-properties?)
     (set-object-property! grob-name 'is-grob? #t)
     (set! ifaces-entry (append (case class
                                  ((Item) '(item-interface))
                                  ((Spanner) '(spanner-interface))
                                  ((Paper_column) '((item-interface
                                                     paper-column-interface)))
                                  ((System) '((system-interface
                                               spanner-interface)))
                                  (else '(unknown-interface)))
                          ifaces-entry))
     (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
     (set! ifaces-entry (cons 'grob-interface ifaces-entry))
     (set! meta-entry (assoc-set! meta-entry 'name grob-name))
     (set! meta-entry (assoc-set! meta-entry 'interfaces
                        ifaces-entry))
     (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
     (set! all-grob-descriptions
           (cons (cons grob-name grob-entry)
             all-grob-descriptions))))

#(define (make-box thick padding xext yext)
   (let ((xext (interval-widen xext padding))
         (yext (interval-widen yext padding)))
   (ly:stencil-add
    (make-filled-box-stencil xext (cons (- (car yext) thick) (car yext)))
    (make-filled-box-stencil xext (cons (cdr yext) (+ (cdr yext) thick)))
    (make-filled-box-stencil (cons (cdr xext) (+ (cdr xext) thick)) yext)
    (make-filled-box-stencil (cons (- (car xext) thick) (car xext)) yext))))

#(define (music-boxer-stencil grob)
   (let* ((elts (ly:grob-object grob 'elements))
          (refp-X (ly:grob-common-refpoint-of-array grob elts X))
          (X-ext (ly:relative-group-extent elts refp-X X))
          (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
          (Y-ext (ly:relative-group-extent elts refp-Y Y))
          (padding (ly:grob-property grob 'padding 0.3))
          (stil (make-box 0.1 padding X-ext Y-ext))
          (offset (ly:grob-relative-coordinate grob refp-X X)))
     (ly:stencil-translate-axis stil (- offset) X)))

#(define box-stil music-boxer-stencil)

#(add-grob-definition
  'Box
  `(
     (stencil . ,box-stil)
     (meta . ((class . Item)
              (interfaces . ())))))

#(add-grob-definition
  'MusicBoxer
  `(
     (stencil . ,music-boxer-stencil)
     (meta . ((class . Spanner)
              (interfaces . ())))))


#(define box-types
   '(
      (BoxEvent
       . ((description . "A box encompassing music at a single timestep.")
          (types . (general-music box-event music-event event))
          ))
      ))

#(define music-boxer-types
   '(
      (MusicBoxerEvent
       . ((description . "Used to signal where boxes encompassing music start and stop.")
          (types . (general-music music-boxer-event span-event event))
          ))
      ))


#(set!
  music-boxer-types
  (map (lambda (x)
         (set-object-property! (car x)
           'music-description
           (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
    music-boxer-types))

#(set!
  box-types
  (map (lambda (x)
         (set-object-property! (car x)
           'music-description
           (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
    box-types))

#(set! music-descriptions
       (append music-boxer-types music-descriptions))

#(set! music-descriptions
       (append box-types music-descriptions))

#(set! music-descriptions
       (sort music-descriptions alist<?))


#(define (add-bound-item spanner item)
   (if (null? (ly:spanner-bound spanner LEFT))
       (ly:spanner-set-bound! spanner LEFT item)
       (ly:spanner-set-bound! spanner RIGHT item)))

musicBoxerEngraver =
#(lambda (context)
   (let ((span '())
         (finished '())
         (current-event '())
         (event-start '())
         (event-stop '()))

     `((listeners
        (music-boxer-event .
          ,(lambda (engraver event)
             (if (= START (ly:event-property event 'span-direction))
                 (set! event-start event)
                 (set! event-stop event)))))

       (acknowledgers
        (note-column-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)
                  (add-bound-item span grob)))
             (if (ly:spanner? finished)
                 (begin
                  (ly:pointer-group-interface::add-grob finished 'elements grob)
                  (add-bound-item finished grob)))))

        (inline-accidental-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))

        (script-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))

        (finger-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))

        ;; add additional interfaces to acknowledge here

        )

       (process-music .
         ,(lambda (trans)
            (if (ly:stream-event? event-stop)
                (if (null? span)
                    (ly:warning "No start to this box.")
                    (begin
                     (set! finished span)
                     (ly:engraver-announce-end-grob trans finished event-start)
                     (set! span '())
                     (set! event-stop '()))))
            (if (ly:stream-event? event-start)
                (begin
                 (set! span (ly:engraver-make-grob trans 'MusicBoxer event-start))
                 (set! event-start '())))))

       (stop-translation-timestep .
         ,(lambda (trans)
            (if (and (ly:spanner? span)
                     (null? (ly:spanner-bound span LEFT)))
                (ly:spanner-set-bound! span LEFT
                  (ly:context-property context 'currentMusicalColumn)))
            (if (ly:spanner? finished)
                (begin
                 (if (null? (ly:spanner-bound finished RIGHT))
                     (ly:spanner-set-bound! finished RIGHT
                       (ly:context-property context 'currentMusicalColumn)))
                 (set! finished '())
                 (set! event-start '())
                 (set! event-stop '())))))

       (finalize
        (lambda (trans)
          (if (ly:spanner? finished)
              (begin
               (if (null? (ly:spanner-bound finished RIGHT))
                   (set! (ly:spanner-bound finished RIGHT)
                         (ly:context-property context 'currentMusicalColumn)))
               (set! finished '())))
          (if (ly:spanner? span)
              (begin
               (ly:warning "unterminated box :-(")
               (ly:grob-suicide! span)
               (set! span '()))))))))


boxEngraver =
#(lambda (context)
   (let ((box '())
         (ev '()))

     `((listeners
        (box-event .
          ,(lambda (engraver event)
             (set! ev event))))

       (acknowledgers
        (note-column-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:grob? box)
                 (begin
                  ; (set! (ly:grob-parent box X) grob) ;; ??
                   (set! (ly:grob-parent box Y) grob)
                 (ly:pointer-group-interface::add-grob box 'elements grob)))))

        (inline-accidental-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:item? box)
                 (ly:pointer-group-interface::add-grob box 'elements grob))))

        (script-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:item? box)
                 (ly:pointer-group-interface::add-grob box 'elements grob))))

        (finger-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:item? box)
                 (ly:pointer-group-interface::add-grob box 'elements grob))))

        ;; add additional interfaces to acknowledge here

        )

       (process-music .
         ,(lambda (trans)
            (if (ly:stream-event? ev)
                (begin
                 (set! box (ly:engraver-make-grob trans 'Box ev))
                 (set! ev '())))))
       (stop-translation-timestep .
         ,(lambda (trans)
            (set! box '()))))))

musicBoxerStart =
#(make-span-event 'MusicBoxerEvent START)

musicBoxerEnd =
#(make-span-event 'MusicBoxerEvent STOP)

				      box = #(make-music 'BoxEvent)
