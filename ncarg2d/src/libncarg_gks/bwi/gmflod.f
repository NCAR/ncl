C
C	$Id: gmflod.f,v 1.3 2000-07-12 16:50:50 haley Exp $
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
      SUBROUTINE GMFLOD(LIST,BITS,COUNT,GKSERR)
C
C  Load the bit string contiained in the low order part of each word
C  into the metafile.
C
C  INPUT
C    LIST  -- A list of words which have bit strings right justified.
C    BITS  -- The number of bits per word to move.
C    COUNT -- The repitition count (number of words in the list).

C  OUTPUT
C    GKSERR -- An error status.
C
C  All data is type integer unless otherwise indicated.
C
      IMPLICIT INTEGER (A-Z)
      DIMENSION LIST(*)
C
C  Common for metafile buffer.
C
      include 'g01prm.h'
      include 'g01io.h'
C
      DATA  ALLOK/0/
C
      IF (COUNT .LE. 0)  RETURN
      CTEMP = COUNT
      STRT = 1
C
C  Determine the number of packets of size BITS left in buffer.
C
   10 CONTINUE
      BLEFT = (MXBITS-MBFPOS)
      REPLFT = BLEFT/BITS
C
C  Compute how may packets to move into the buffer.
C
      IF (CTEMP .LE. REPLFT) THEN
C
C  Room for all.
C
        CMOVE = CTEMP
        CTEMP = 0
      ELSE
C
C  Not enough room for all bits.
C
        CMOVE = REPLFT
        CTEMP = CTEMP - REPLFT
      END IF
C
C  Move the current bit packets.
C
      CALL SBYTES(MOUTBF,LIST(STRT),MBFPOS,BITS,0,CMOVE)
      MBFPOS = MBFPOS + (BITS * CMOVE)
C
C  Check if more bit packets remain.
C
      IF (CTEMP .NE. 0) THEN
C
C  More remaining, flush the current buffer.
C
        CALL G01FLB(GKSERR)
        IF (GKSERR .NE. ALLOK) RETURN
        STRT = STRT + CMOVE
        GO TO 10
      END IF
C
      RETURN
      END
