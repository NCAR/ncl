C
C	$Id: sfblda.f,v 1.3 1992-09-04 20:46:35 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      BLOCK DATA SFBLDA
C
C Specify default values for the internal parameters.
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,LCH,LDP(8,8)
C
C AID is the angle at which fill lines are to be drawn - a real value,
C in degrees.
C
      DATA AID / 0. /
C
C DBL is the distance between fill lines - a real value, in normalized
C device coordinate units, between 0 and 1.  Values less than zero or
C greater than 1 act the same as .00125, which is also the default.
C
      DATA DBL / .00125 /
C
C ITY is a flag specifying the type of fill to be done by the routine
C SFSGFA.
C
      DATA ITY / 0 /
C
C LPA is the pattern selector - the value 0 means that solid lines are
C to be used, the value 1 that dotted lines are to be used (the dots to
C be arranged according to the pattern specified by "LDP").
C
      DATA LPA / 0 /
C
C If LCH is 0, the dots in a dotted line will be actual dots.  If LCH
C is non-zero, the character specified by LCH will be used instead of
C the dots.  See the plot package routine "POINTS".
C
      DATA LCH / 0 /
C
C LDP contains a set of 0s and 1s defining the current dot pattern, in
C an 8x8 pixel.  The value 1 turns a dot on, the value 0 turns it off.
C
      DATA LDP / 64*1 /
C
      END
