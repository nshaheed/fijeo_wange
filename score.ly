\version "2.19.80"
\language "english"
\include "defn.ily"
				% \include "arrows.ily"
% #(set! paper-alist (cons '("my size" . (cons (* 11 in) (* 8.5 in))) paper-alist))
% #(set-default-paper-size "letter" 'landscape)
#(set-default-paper-size "letterlandscape")

\paper {
				% #(set-paper-size "my size")
  top-margin = 0.3\in
  bottom-margin = 0.3\in
  left-margin = 0.6\in
  right-margin = 0.6\in
}

\header {
  composer = \markup { \column {
    \cmp
    {\vspace #-0.5 " " }
  }}
  title = \ttl
  subtitle = \subttl
  copyright = \yr
  tagline = ##f
}

violinone = \relative c'' {
  \override Staff.TimeSignature #'stencil = ##f
  \override Staff.Clef.full-size-change = ##t
  \autoBeamOff
  \arpeggioParenthesis
  \override DynamicLineSpanner.staff-padding = #2.2
  \override TupletBracket.bracket-visibility = ##f
  \override TupletNumber.stencil = ##f
  \set Score.markFormatter = #format-mark-box-alphabet
  \override Score.RehearsalMark.X-offset = #-1
  \override Hairpin.to-barline = ##f
				% \override Score.RehearsalMark.Y-offset = #1
  % \override Score.RehearsalMark.padding = #2

  \tempo \markup {
    \column {
      \concat {
	\smaller \general-align #Y #DOWN \note #"16" #1
	" = "
	\smaller "extremely quickly"
      }
      { \vspace #-0.2 \smaller "Intense, frantic" }
      % { \vspace #-1 " "}
     }
  }
  \once \override Staff.MeasureCounter.outside-staff-priority = #2000
  \timeBracket "30" {
    \time 3/16
    \cell {
      e16-.->\pp
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
      e16-.->[ e16-. e16-.]
    }

    \time 1/16
    \beginArrow
    s16\<\startTextSpan\breathe
    s16
    s16\stopTextSpan
				% s8 s8.
				% s8. s8 s16\stopTextSpan
    \arrowStop
				% \break
    \time 6/16
    \cell {
      <e f>16-.->\p
      <e f>16-.->[ <e f>16-.->]
      <e f>16-.->[ <e f>16-. <e f>16-.]
  }
				% \time 3/16
    \time 1/16
    \beginArrow
    s16\startTextSpan\breathe
    s16*7
				% s8 s8. s8.
				% s8. s8
    s16\stopTextSpan
    \arrowStop

    \time 7/16
    \cell {
      <e f gf>16-.->
      <e f gf>16-.->[ <e f gf>16-.->]
      <e f gf>16-.->[ <e f gf>16-. <e f gf>16-.]
      e'16-.
    }

    \time 1/16
    \beginArrow s16\<\startTextSpan\breathe
    s16*8

    s16
				% \once \textLengthOn  
    s16^"  "\stopTextSpan
    \arrowStop
  }

  \break

  \timeBracket "20" {
    \time 11/16
    \cell {
      \mark \default
      <e, f gf>16-.->\mp
      <e f gf>16-.->[ <e f gf>16-.->]
      <e f gf>16-.->[ <e f gf>16-. <e f gf>16-.]
      e'16-.
      \trem_duo
      <e, f>1*4/16-^\>:32^\markup {\hspace #-.7 \sp_paren}
    }
    
    \time 1/16
    \beginArrow s16\!\startTextSpan\breathe
    s16*23
    s16*2\<
    s16\stopTextSpan
    \arrowStop
  }
  \break

  \override Staff.MeasureCounter.staff-padding = #8
  \timeBracket "40" {
    \time 12/16
    \mark \default
    \cell {
      <e f gf>16-.->\mf
      <e f gf>16-.->[ <e f gf>16-.->]
      e'16-.
      \tuplet 4/3 {
	e16-.[ e16-. e16-. e16-.]
      }
      \trem_duo
      <e, f>1*5/16:32-^\>^\markup {\hspace #-1.5 \sp_paren}
    }

    \time 1/16
    \beginArrow s16\!\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop

    \time 13/16
    \cell {
      \tuplet 12/13 { 
	<e f gf>16-.->\f
	e'16-.->
	<e>16-.->[ <e>16-.->]    
	e16-.->[ e16-. e16-. e16-.]
	\trem_duo
	<e, f>1*4/16:32-^\>^\markup {\hspace #-1.5 \sp_paren}
      }
    }
    
    \time 1/16
    \beginArrow s16\!\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
    
    \time 9/16
    \cell {
      \tuplet 8/9 {
	<e f gf>16-.->\f
	e'16-.->
	<e>16-.->[ <e>16-.->]    
	e16-.->[ e16-. e16-. e16-.]
      }
    }

    \time 1/16
    \beginArrow s16\!\<\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
  }
  
  \bar "||"
  \timeBracket "5" {
    \time 9/8
    e1:32\fermata\<\ff
    ef,8-^\!\caesura
  }
  \revert Staff.MeasureCounter.staff-padding

  \time 3/16
  \break
  \once \override Staff.MeasureCounter.outside-staff-priority = #2000
  \override Staff.MeasureCounter.staff-padding = #10.5
  % \once \override Staff.MeasureCounter.Y-extent = #3
  \tempo "maintain speed, but more relaxed"
  \timeBracket "15" {
    \mark \default
    \cellSplit {
      ef16-_\p^\markup {
      \column {
	{ \tiny " "  }
	\sp
	}}
      ef-_[ ef-_]
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop

    \time 4/16
    \cell {
      ef16-_
      ef-_[ ef-_]
      bf'16-_
    }

    
    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
  }
  s16\stopTextSpan
  \arrowStop  
  
  \once \override Staff.MeasureCounter.outside-staff-priority = #2000
  \timeBracket "25" {
    \time 6/16
    \cell {
      ef,16-_
      ef-_[ ef-_]
      \once \textLengthOn
      bf'16-_^\markup { \hspace #1 " "}
      \override Box.color = #(rgb-color 0.0 0.9 0.0)

      \once \override Arpeggio.positions = #'(2 . 5)
      b1*1/16*2--\arpeggio^\markup{ \hspace #-2.5 \norm_espr_paren }
    }
    
    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop

    \time 8/16
    \cell {
      ef,16-_
      ef-_[ ef-_]
      bf'16-_
      \once \override Score.Box.padding = 0.6
      \once \override Arpeggio.padding = 0
      \once \override Arpeggio.positions = #'(2 . 6)
      <b ds>1*2/16*2--\arpeggio^\markup{ \hspace #-1.2 \norm_espr_paren }
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
  }
  \revert Staff.MeasureCounter.staff-padding
  
  \break
  \timeBracket "30" {
    \mark \default
    \time 10/16
    \cell {
      \once \override Arpeggio.positions = #'(1 . 6)
      \once \override Arpeggio.padding = 0
      <gs as b ds>1*5/16*2--\arpeggio^\markup{ \hspace #-4 \norm_espr }
    }

    \time 1/16
    \beginArrow_no_comma

    % \once \override TextSpanner.extra-offset = #'(0 . -3.1)
    % \revert TextSpanner.break-visibility
				% \bar "|"
    % \once \override Hairpin.to-barline = ##f
    s16\startTextSpan\<
    s16*2
    \once \override DynamicText.X-offset = #0
    s16*8\mp
    s16\stopTextSpan
    \arrowStop

    \time 5/16
    \cell {
      \once \override Score.Box.padding = 0.6
				% \once \override Score.Box.offset = 0.5    
				% \box
      \once \hide Stem
      \once \override Script.padding = #0.5      
      d'?4*5/4--\fermata\lv\<\caesura
      \once \override Staff.BarLine.allow-span-bar = ##f
      \bar "|"
    }

    % \time 1/16
    % \beginArrow_no_comma s16\startTextSpan
    % s16
    % \once \override TextScript.extra-offset = #'(0 . -3)  
    % s16\stopTextSpan^\markup{
    %   \caesuraMk
    % }
    % \arrowStop
  }
  \break
  
  \override Staff.MeasureCounter.staff-padding = #8.5
  \timeBracket "60" {
    \mark \default
    \time 2/16
    \cell {
      \ottava #1
      \once \override Arpeggio.positions = #'(1.5 . 6)
      \once \override Arpeggio.padding = 0
      \once \override Staff.OttavaBracket.shorten-pair = #'(-3 . -2.5)
      <a' bf cf>1*2/16--\arpeggio\mf
      \ottava #0
    }

    \time 1/16
    \beginArrow_no_comma s16\startTextSpan
    s16*19
    s16*2\<
  }
  s16\stopTextSpan
  \arrowStop
  
  \timeBracket "7" {
    \time 4/4
    \once \override Staff.OttavaBracket.shorten-pair = #'(-1 . -2.5)
    \ottava #1
    a1:32\fermata\f
    \ottava #0
    \once \override Staff.BarLine.allow-span-bar = ##f
    \bar "||"
  }
  \revert Staff.MeasureCounter.staff-padding
  
  \pageBreak

  \timeBracket "15" {
    \time 3/16
    \mark \default
    \tempo "like beginning"
    \once \override Staff.MeasureCounter.outside-staff-priority = #2000    
    \cell {
      e,16-.->\p
      e16-.->[ e16-.->]
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
    
    % \time 4/16
    \time 7/16
    \tuplet 4/7 {
    \cell {
      <e f>16-.->
      <e f>16-.->[ <e f>16-.->]
      e'16->
    }
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop

    \time 11/16
    \cell {
      \tuplet 8/11 {
      <e, f>16-.->
      <e f>16-.->[ <e f>16-.->]
      e'16->
      e16->[ e e e]
      }
    }
    
    \time 1/16
    \beginArrow s16\<\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
  }

  \break

  \timeBracket "10" {
    \time 13/16
    \cell {
      e,16-.->\mf
      e16-.->[ e16-.->]
				% \override Score.SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/64)
      \ottava #1
      <e' f gf>16->
      <e f gf>16-> [<e f gf> <e f gf> <e f gf>
		  ]
      <e f gf>-> [<e f gf> <e f gf> <e f gf> <e f gf>
		]
      \ottava #0
    }
    
    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16*13
    s16\stopTextSpan
    \arrowStop
  }

  \timeBracket "15" {
    \time 14/16
    \mark \default
    \cell {
      \ottava #1
      e,16-.->
      <e' f gf>16->
      <e f gf>16-> [<e f gf> <e f gf> <e f gf> <e f gf>
		  ]
      <e f gf>-> [<e f gf> <e f gf> <e f gf> <e f gf> <e f gf>
		]
      e'16-.->      
      \ottava #0
    }

    \time 1/16
    \beginArrow s16\<\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
    
    \time 16/16
    \cell {
      \ottava #1
      \tuplet 14/16 {
	e16-.->\f
	e16->-.[ e->-.]
	<e, f gf>16-> [<e f gf> <e f gf> <e f gf> <e f gf>
		     ]
	<e f gf>-> [<e f gf> <e f gf> <e f gf> <e f gf> <e f gf>
		  ]
      }
      \ottava #0
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
  }
  
  \pageBreak

  \timeBracket "10" {
    \time 18/16
    \cell {
      \ottava #1
      <e f gf>16-> [<e f gf> <e f gf> <e f gf> <e f gf>
		  ]
      <e f gf>-> [<e f gf> <e f gf> <e f gf> <e f gf> <e f gf>
		]
      e'-> [e e e e e e
	  ]
      \ottava #0
    }
    
    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16*20
    s16\stopTextSpan
    \arrowStop
  }
  
  \break

  \timeBracket "10" {
    \mark \default
    \time 15/16
    \cell {
      \ottava #1
      e-> [e e e e e e
	 ]
      e-> [e e e e e e e
	 ]
      \ottava #0
    }

    \time 1/16
    \beginArrow s16\<\startTextSpan\breathe
    s16*22
    s16\stopTextSpan
    \arrowStop
  }
  \break

  \override Staff.MeasureCounter.staff-padding = #6
  \timeBracket "20" {
    \time 7/16
    \cell {
      <e,, f gf>->\ff [<e f gf> <e f gf> <e f gf> <e f gf> <e f gf> <e f gf>
		  ]
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop

    \time 7/16
    \cell {
      <e, f>-> [<e f> <e f> <e f> <e f> <e f> <e f>
	      ]
    }

    \time 1/16
    \beginArrow s16\startTextSpan\breathe
    s16
    s16\stopTextSpan
    \arrowStop
  }

  \timeBracket "15" {
    \bar "||"
    \time 17/16
    \ottava #2
    e'''1:32\< f16-^\fff
    \ottava #0
    \bar "|."
  }
  
}

violintwo = \relative c'' {
  \override Staff.TimeSignature #'stencil = ##f
  \override Staff.Clef.full-size-change = ##t
  \override DynamicLineSpanner.staff-padding = #2.2
  \override TupletBracket.bracket-visibility = ##f
  \override TupletNumber.stencil = ##f
  \override Hairpin.to-barline = ##f
  
  \arpeggioParenthesis
  \autoBeamOff
  \time 3/16
  \cell {
    e16-.->\pp e16-.->[ e16-.->]
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
  \beginArrow s16\<\startTextSpan\breathe
  s16 s16
  s16*7 s16\stopTextSpan
  % s8
  % s8. s8. s8. s8. s8.
  % s8 s16\stopTextSpan
  \arrowStop

  % \clef G
  \time 4/16
  \cell {
    <ds e>16-.->\p
    <ds e>16-.->[ <ds e>16-.->]
    e4*1/4--
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  % s8
  % s8. s8. s8. s8.
				% s8
  \once \textLengthOn
  s16^""
  s16*9
  s16\stopTextSpan
  \arrowStop

  \time 7/16
  \cell {
    <d! ef ff>16-.->
    <d ef ff>16-.->[ <d ef ff>16-.->]
    <d ef ff>16-.->[ <d ef ff>16-. <d ef ff>16-.]    
    e'16-.
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  % \once \textLengthOn
  s16\stopTextSpan
  \once \override TextSpanner.extra-offset = #'(0 . -3.1)  
  s16\<\startTextSpan
  s16*11
  s16\stopTextSpan
  \arrowStop

  \clef "treble"
  \time 11/16
  \cell {
    <d, ef ff>16-.->\mp
    <d ef ff>16-.->[ <d ef ff>16-.->]
    <d ef ff>16-.->[ <d ef ff>16-. <d ef ff>16-.]    
    e'16-.
    \trem_duo
    <ef,! ff!>1*4/16:32-^\>^\markup {\hspace #-1.5 \sp_paren}
  }

  \time 1/16
  \beginArrow s16\!\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 8/16
  \cell {
    <d ef ff>16-.->
    <d ef ff>16-.->[ <d ef ff>16-.->]
    <d ef ff>16-.->[ <d ef ff>16-. <d ef ff>16-.]
    e,16-.
    e''16-.
  }

  \time 1/16
  \beginArrow s16\<\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  % \time 11/16
  % \cell {
  %   <d, e>16-.->
  %   <d e>16-.->[ <d e>16-.->]
  %   e,16-.[ e-. e-.]
  %   e''16-.
  %   <d, e>1*4/16:32-^\>
  % }

  
  \time 12/16
  \cell {
    \tuplet 11/12
    {
      <d, ef ff>16-.->\mf
      <d ef ff>16-.->[ <d ef ff>16-.->]
      e,16-.[ e-. e-.]
      e''16-.
      \trem_duo
      <ef,! ff!>1*4/16:32-^\>^\markup {\hspace #-1.5 \sp_paren}
    }
  }

  
  \time 1/16
  \beginArrow s16\!\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  
  \time 13/16
  \cell {
    <d ef ff>16-.->\f
    e,16-.[ e-. e-.]
    e16-.[ e-. e-. e-.]    
    e''16->-.
    \trem_duo
    <ef,! ff!>1*4/16:32-^\>^\markup {\hspace #-1.5 \sp_paren}
  }

  \time 1/16
  \beginArrow s16\!\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 9/16
  \cell {
    <d ef ff>16-.->
    e,16-.[ e-. e-.]
    e''16-.->
    e16->-.[ e-. e-. e-.]        
  }

  \time 1/16
  \beginArrow s16\!\<\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop
  

  \bar "||"
  \time 9/8
  e1:32\fermata\<\ff
  \appoggiatura e,,16
  ef'8-^\!\caesura

  \time 3/16
  \break
  \cellSplit {
    ef16-_\p^\sp
    ef-_[ ef-_]
  }

  
  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 4/16
  \cell {
    ef16-_
    ef-_[ ef-_]
    af,16-_
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 6/16
  \cell {
    \tuplet 4/6 {
      ef'16-_
      ef-_[ ef-_]
      <af, af'>16-_
    }
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 8/16
  \cell {
    ef'16-_
    ef-_[ ef-_]
    <af, af'>16-_
    af16[ af af af]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 10/16
  \cell {
    ef'16-_[
    ef-_ ef-_]
    af16-_[ af-_ af-_]
    af,16[ af af af]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\<\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 2/16
  \cell {
    \once \override Score.Box.padding = 0.6
    % \once \override Score.Box.offset = 0.5    
				% \box
    \once \override Arpeggio.positions = #'(1 . 6)
    \once \override Arpeggio.padding = 0
    \once \override TextScript.extra-offset = #'(-12.2 . -3)
    <gs' as b ds>1*2/16--\arpeggio\mp    
    ^\markup{ \norm_espr_col }
  }

  \time 1/16
  \beginArrow_no_comma s16\startTextSpan
  s16
  s16\stopTextSpan
  \arrowStop

  
  \time 9/16
  \cell {
    \once \hide Stem
    \once \override Script.padding = #1
    e,?4*9/4--\fermata\lv\<\caesura
    \bar "|"
  }

  
  % \time 1/16
  
  % % \once \override TextSpanner.bound-details.right.text = \caesuraMk
  % % \alterBroken bound-details.right.text \caesuraMk TextSpanner
  % \beginArrow_no_comma s16\startTextSpan\<
  % s16*5
  % \once \override TextScript.extra-offset = #'(0 . -3)
  % s16\stopTextSpan^\markup{
  %     \caesuraMk
  %   }
  % \arrowStop


  \time 2/16
  \cell {
    \once \override Arpeggio.positions = #'(1 . 5)
    \once \override Arpeggio.padding = 0     
    \once \override Staff.OttavaBracket.shorten-pair = #'(-4 . -4)
    \ottava #1
    <<
      < g'' af> 1*2/16\arpeggio
      \new Voice { \tweak X-offset #-1.6 a!-- }
      {\once \override DynamicText.X-offset = #-2.7
	s1*2/16\mf}
    >>
    % <g'' af \tweak X-offset #1.7 a!>1*2/16--
    \ottava #0
  }

  \time 1/16
  \beginArrow_no_comma s16\startTextSpan
  s16
  s16\stopTextSpan
  \arrowStop

  \once \override Arpeggio.positions = #'(1 . 5)
  \once \override Arpeggio.padding = 0     
  \time 2/16
  \cell {
    <<
      < g, af> 1*2/16\arpeggio
      \new Voice { \tweak X-offset #-1.5 a!-- }
    >>    
    % <g, af a>1*2/16--
  }

  \time 1/16
  \beginArrow_no_comma s16\startTextSpan
  s16
  s16\stopTextSpan
  \arrowStop

  \time 2/16
  \cell {
    \once \override Arpeggio.positions = #'(1 . 5)
    \once \override Arpeggio.padding = 0         
    \once \override Staff.OttavaBracket.shorten-pair = #'(-4 . -4)    
    \ottava #1
    <<
      < g' af> 1*2/16\arpeggio
      \new Voice { \tweak X-offset #-1.5 a!-- }
    >>        
    \ottava #0
  }

  
  \time 1/16
  \beginArrow_no_comma s16\startTextSpan
  s16
  s16\stopTextSpan
  \arrowStop

  \time 2/16
  \cell {
    \once \override Arpeggio.positions = #'(-2 . 1)
    \once \override Arpeggio.padding = 0    
    <<
      <g,, af> 1*2/16\arpeggio
      \new Voice { \tweak X-offset #-3.8 a!-- }
    >>            
    % <g,, af a>1*2/16--
  }

  \time 1/16
  \beginArrow_no_comma s16\startTextSpan
  s16
  s16\stopTextSpan
  \arrowStop

  
  \time 2/16
  \cell {
    \once \override Arpeggio.positions = #'(1 . 5)
    \once \override Arpeggio.padding = 0         
    \once \override Staff.OttavaBracket.shorten-pair = #'(-4 . -4)        
    \ottava #1
    <<
      < g'' af> 1*2/16\arpeggio
      \new Voice { \tweak X-offset #-1.5 a!-- }
    >>            
    % <g'' af a>1*2/16--
    \ottava #0
  }

  
  \time 1/16
  \beginArrow_no_comma s16\startTextSpan\<
  s16
  s16\stopTextSpan
  \arrowStop

  \time 4/4
  \once \override Staff.OttavaBracket.shorten-pair = #'(-1 . -2.5)  
  \ottava #1
  gs1:32\fermata\f
  \ottava #0
  \bar "||"

  \time 3/16
  \cell {
    e,16-.->\p
    e16-.->[ e16-.->]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop
  
  \time 7/16
  \cell {
    <ds e>16-.->
    <ds e>16-.->[ <ds e>16-.->]
    \once \override Beam.grow-direction = #LEFT
    \once \override Script.avoid-slur = #'inside
    \once \override Script.outside-staff-priority = ##f
    \tuplet 5/4 {
      e16-_^\markup{ \jete, \sp_paren }
      ([ e-_ e-_ e-_ e-_
       )]
    }
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop
  
  \time 11/16
  \cell {
    <ds e>16-.->
    <ds e>16-.->[ <ds e>16-.->]
    \once \override Beam.grow-direction = #LEFT
    \once \override Script.avoid-slur = #'inside
    \once \override Script.outside-staff-priority = ##f
				% \once \override Script.slur-padding = #
    \tuplet 5/4 {
      e-_^\markup{ \jete, \sp_paren}([ e-_ e-_ e-_ e-_)]
    }
    e,16->[ e16 e16 e16]
  }

  \time 1/16
  \beginArrow s16\<\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop
  
  \time 13/16
  \cell {
    \tuplet 12/13 {
      <ds' e>16-.->\mf
      <ds e>16-.->[ <ds e>16-.->]
      \once \override Beam.grow-direction = #LEFT
      \once \override Script.avoid-slur = #'inside
      \once \override Script.outside-staff-priority = ##f
      \tuplet 5/4 {
	e-_^\markup{\jete, \sp_paren } ([ e-_ e-_ e-_ e-_)]
      }
      e,16->[ e16 e16 e16]
      e''->
    }
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop
  
  \time 9/16
  \cell {
    e,16->-.
    e,16->[ e e e
	]
    <d'' ef ff>->
    [ <d ef ff> <d ef ff> <d ef ff>
    ]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 14/16
  \cell {
    \tuplet 10/14 {
    e,16->-.
    e,16->[ e e e
	]
    <d'' ef ff>->
    [ <d ef ff> <d ef ff> <d ef ff> <d ef ff>
    ]
    }
  }

  \time 1/16
  \beginArrow s16\<\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 16/16
  \cell {
    % <ds e>16-.->
				% <ds e>16-.->[ <ds e>16-.->]
    \once \override Staff.OttavaBracket.outside-staff-priority = #8
    \ottava #1
    <d ef ff>16\f->^\markup { \translate #'(0 . 7) " " }
    [
      <d ef ff> <d ef ff> <d ef ff> <d ef ff>
    ]
    \once \override Beam.grow-direction = #LEFT
    \once \override Script.avoid-slur = #'inside
    \once \override Script.outside-staff-priority = ##f    
    \tuplet 5/4 {
      e,-_^\jete([ e-_ e-_ e-_ e-_)]
    }
    <d' ef ff>16->-.[
      <d ef ff> <d ef ff> <d ef ff> <d ef ff> <d ef ff>
    ]
    e'->-.
    \ottava #0
  }

  \time 1/16
  \beginArrow_no_comma s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  s16\startTextSpan
  s16*19
  s16\stopTextSpan

  % \beginArrow s16\startTextSpan\breathe
  % s16*22
  % s16\stopTextSpan
  \arrowStop

  \time 16/16
  \set Staff.forceClef = ##t 
  \clef treble
  \cell {
    \once \override Staff.OttavaBracket.outside-staff-priority = #8
    \ottava #1
    <d, ef ff>16->
    [
      <d ef ff> <d ef ff> <d ef ff> <d ef ff> <d ef ff>
    ]
    e,16-.->
    e16-.->[ e16-.->]    
    e''16->[
      e e e e e e
    ]
    \ottava #0
  }

  \time 1/16
  \beginArrow_no_comma s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  s16\startTextSpan
  s16*14
  s16*2\<
  s16\stopTextSpan
  \arrowStop
  % \break

  \time 18/16
  \set Staff.forceClef = ##t   
  \clef treble
  \cell {
    \once \override Staff.OttavaBracket.outside-staff-priority = #8
    \ottava #1
    e,,16-.->^\markup { \translate #'(0 . 7) " " }
    e16-.->[ e16-.->]
    e''-> [e e e e e e
	 ]
    e-> [e e e e e e e
     ]
    \ottava #0
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 7/16
  \cell {
    <d,, ef ff>->\ff [<d ef ff> <d ef ff> <d ef ff> <d ef ff> <d ef ff> <d ef ff>
	 ]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \time 7/16
  \cell {
    <ds, e>-> [<ds e> <ds e> <ds e> <ds e> <ds e> <ds e>
	 ]
  }

  \time 1/16
  \beginArrow s16\startTextSpan\breathe
  s16
  s16\stopTextSpan
  \arrowStop

  \bar "||"
  \time 17/16
  \ottava #2
  e'''1:32\< e16-^\fff
  \bar "|."
  \ottava #0
  % \time 1/16
  % \beginArrow s16\startTextSpan\breathe
  % s16
  % s16\stopTextSpan
  % \arrowStop

}

\score {
  <<
    \set Timing.defaultBarType = "|-nothing"
    % \new ChoirStaff <<
    \new StaffGroup <<      
      \new Staff <<
	\set Staff.instrumentName = "Violin 1"
	\violinone
      >>
      \new Staff <<
	\set Staff.instrumentName = "Violin 2"
	\violintwo
      >>
    >>
  >>
  \layout {
    \context {
      \Global
      % \grobdescriptions #all-grob-descriptions
    }
    \context {
      \Score
      \remove "Timing_translator"
      \remove "Default_bar_line_engraver"
    }
    \context {
      \Staff
      \consists "Timing_translator"
      \consists "Default_bar_line_engraver"
      \consists #Measure_attached_spanner_engraver
      \override MeasureCounter.font-encoding = #'latin1
      \override MeasureCounter.font-size = 0
      \override MeasureCounter.outside-staff-padding = 2
      \override MeasureCounter.outside-staff-horizontal-padding = #0
    }
    \context {
      \Voice
      % \consists \musicBoxerEngraver % for spans
      % \consists \boxEngraver
				%   default settings:
      % \override MusicBoxer.layer = -10
      % \override Box.layer = -10
    }    
  }
}



