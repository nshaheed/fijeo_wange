\version "2.19.80"
\language "english"
\include "defn.ily"
% \include "arrows.ily"

\header {
  composer = \cmp
  title = \ttl
  % subtitle = \sbttl
  copyright = \yr
  tagline = ##f
}

violinone = \relative c'' {
  \override Staff.TimeSignature #'stencil = ##f
  \override Staff.Clef.full-size-change = ##t

  \time 3/16
  \cell {
    e16-.->
    e16-.->[ e16-.->]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16 s16\stopTextSpan
  % s8 s8.
  % s8 s16\stopTextSpan
  \arrowStop
  
  \time 6/16
  \cell {
    e16-.->
    e16-.->[ e16-.->]
    e16-.->[ e16 e16]
  }

  \time 1/16
  \beginArrow
  s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  % s8 s8.
  % s8. s8 s16\stopTextSpan
  \arrowStop

  \time 6/16
  \cell {
    <e f>16-.->
    <e f>16-.->[ <e f>16-.->]
    <e f>16-.->[ <e f>16 <e f>16]
  }

				% \time 3/16
  \time 1/16
  \beginArrow
  s16\startTextSpan\breathe
  s16*8
				% s8 s8. s8.
				% s8. s8
  s16\stopTextSpan
  \arrowStop

  \time 7/16
  \cell {
    <e f>16-.->
    <e f>16-.->[ <e f>16-.->]
    <e f>16-.->[ <e f>16 <e f>16]
    e'16-.
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16*9
  s16\stopTextSpan
  \arrowStop

  \time 11/16
  \cell {
    <e, f>16-.->
    <e f>16-.->[ <e f>16-.->]
    <e f>16-.->[ <e f>16 <e f>16]
    e'16-.
    <e, f>1*4/16:32-^\>
  }
  
  \time 1/16
  \beginArrow s16\!\startTextSpan\breathe
  s16*23
  s16\stopTextSpan
  \arrowStop


  \time 12/16
  \cell {
    <e f>16-.->
    <e f>16-.->[ <e f>16-.->]
    e'16-.
    e16-.[ e16-. e16-. e16-.]
    <e, f>1*4/16:32-^\>
  }

  \time 1/16
  \beginArrow s16\!\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop
  
  
}

violintwo = \relative c'' {
  \override Staff.TimeSignature #'stencil = ##f
  \override Staff.Clef.full-size-change = ##t
  \time 3/16
  \cell {
    e16-.-> e16-.->[ e16-.->]
  }

  \time 1/16
  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16 s16\stopTextSpan  
  % \beginArrow s16\startTextSpan\breathe
  % s8 s8.
  % s8 s16\stopTextSpan
  \arrowStop
  
  \time 6/16
  \cell {
    e16-.->
    e16-.->[ e16-.->]
    e4*3/4--
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16 s16
  s16*7 s16\stopTextSpan
  % s8
  % s8. s8. s8. s8. s8.
  % s8 s16\stopTextSpan
  \arrowStop

  % \clef G
  \time 6/16
  \cell {
    <ds e>16-.->
    <ds e>16-.->[ <ds e>16-.->]
    e4*3/4--
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  % s8
  % s8. s8. s8. s8.
				% s8
  s16*9
  s16\stopTextSpan
  \arrowStop

  \time 7/16
  \cell {
    <d e>16-.->
    <d e>16-.->[ <d e>16-.->]
    <d e>16-.->[ <d e>16<d e>16]    
    e'16-.
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16*13
  s16\stopTextSpan
  \arrowStop

  \time 11/16
  \cell {
    <d, e>16-.->
    <d e>16-.->[ <d e>16-.->]
    <d e>16-.->[ <d e>16<d e>16]    
    e'16-.
    <d, e>1*4/16:32-^\>    
  }

  \time 1/16
  \beginArrow s16\!\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 8/16
  \cell {
    <d e>16-.->
    <d e>16-.->[ <d e>16-.->]
    <d e>16-.->[ <d e>16<d e>16]
    e,16-.
    e''16-.
    
  }
  


}

\score {
  
  <<
    \new ChoirStaff <<
      \new Staff {
	\violinone
      }
      \new Staff {
	\violintwo
      }
    >>
  >>
  \layout {
    \context {
      \Score
      \remove "Timing_translator"
      \remove "Default_bar_line_engraver"
    }
    \context {
      \Staff
      \consists "Timing_translator"
      \consists "Default_bar_line_engraver"
    }
  }
}



