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

cmp = \markup {
  \override \fnt
  \smallCaps
  "Nicholas Shaheed"
}


ttl = \markup {
  \override \fnt
  \smallCaps
  "NMOP fjeiowange"
}

yr = \markup {
  \override \fnt
  \smallCaps
  2018
}

\defineBarLine "[" #'("" "[" "")
\defineBarLine "]" #'("]" "" "")

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
  \override TextSpanner.extra-offset = #'(0 . -3.1)
  % \override TextSpanner.break-visibility = #all-invisible  
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
}

cell =
#(define-music-function
  (parser location mus)
  (ly:music?)
  #{
  \bar "["
  #mus
  \bar "]"
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