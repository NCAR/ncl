C
C $Id: sfblda.f,v 1.6 2000-08-22 15:05:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
