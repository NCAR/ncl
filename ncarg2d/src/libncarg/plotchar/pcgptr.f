C
C $Id: pcgptr.f,v 1.8 2000-07-12 16:24:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PCGPTR (KCHR,LCHR,INDX,NLCH,IPNT)
C
C This routine finds the character KCHR in the array LCHR, which has
C been sorted in ascending order, and returns the value of the index,
C from the parallel array INDX, in IPNT.
C
      CHARACTER*1 LCHR(NLCH),KCHR
      DIMENSION   INDX(NLCH)
C
C A binary-halving technique is used.  It is assumed that LCHR is short
C enough so that 10 steps will suffice; if KCHR is not found in that
C many steps, the search is stopped and IPNT, which is used to count
C down the steps, ends up zeroed to indicate what happened.
C
      IBOT=1
      ITOP=NLCH
      IPNT=10
      ITRY=ITOP
      GO TO 102
  101 IF (IPNT.EQ.0) GO TO 106
      IPNT=IPNT-1
      ITRY=(IBOT+ITOP)/2
  102 IF (ICHAR(LCHR(ITRY))-ICHAR(KCHR)) 103,105,104
  103 IBOT=ITRY
      GO TO 101
  104 ITOP=ITRY
      GO TO 101
  105 IPNT=INDX(ITRY)
C
C Done.
C
  106 RETURN
C
      END
