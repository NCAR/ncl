C
C $Id: mprdcs.f,v 1.4 2000-07-12 16:23:44 haley Exp $
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
      SUBROUTINE MPRDCS (IFDE,CHRS,LCHR,MCHR,NCHR,CVAR)
C
        CHARACTER*1 CHRS(LCHR)
C
        CHARACTER*(*) CVAR
C
C Given the file descriptor of an open file in IFDE and a character
C buffer CHRS, of length LCHR, having in it MCHR characters read from
C the file, of which NCHR have previously been processed, this routine
C returns the next LEN(CVAR) characters in the character variable CVAR.
C
C Find the length of the character variable.
C
        LOCV=LEN(CVAR)
C
C Define an index into the character variable.
C
        IICV=0
C
C Transfer LOCV characters to the character variable.
C
  101   NCHR=NCHR+1
C
        IF (NCHR.GT.MCHR) THEN
          CALL NGRDCH (IFDE,CHRS,LCHR,ISTA)
          IF (ISTA.LE.0) THEN
            MCHR=0
            RETURN
          ELSE
            MCHR=ISTA
            NCHR=1
          END IF
        END IF
C
        IICV=IICV+1
        CVAR(IICV:IICV)=CHRS(NCHR)
        IF (IICV.EQ.LOCV) RETURN
C
        GO TO 101
C
      END
