C
C $Id: pcsort.f,v 1.9 2000-08-22 15:05:27 haley Exp $
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
      SUBROUTINE PCSORT (LCHR,INDX,NLCH)
C
C This routine sorts the character array LCHR, which is NLCH long, into
C ascending order.  The elements in the integer array INDX are sorted
C along with LCHR, so that the relationship between them is maintained.
C A bubble sort is used.
C
      CHARACTER*1 LCHR(NLCH),CTM1,CTM2
      DIMENSION   INDX(NLCH)
C
C Initialize the first pass.
C
      IBEG=1
      IEND=NLCH
      ISTP=1
C
C At most NLCH-1 passes are needed.
C
      DO 102 NPSS=1,NLCH-1
C
        IDNE=1
        I=IBEG
C
C If adjacent elements need to be switched, do it.
C
  101   IF (ISTP*(ICHAR(LCHR(I))-ICHAR(LCHR(I+ISTP))).GT.0) THEN
          IDNE=0
          CTM1=LCHR(I)
          CTM2=LCHR(I+ISTP)
          LCHR(I)=CTM2
          LCHR(I+ISTP)=CTM1
          ITMP=INDX(I)
          INDX(I)=INDX(I+ISTP)
          INDX(I+ISTP)=ITMP
        END IF
C
C If the pass isn't done, move to the next set of adjacent elements.
C
        I=I+ISTP
        IF (I.NE.IEND) GO TO 101
C
C Pass done; if none were switched during this pass, we can quit.
C
        IF (IDNE.NE.0) RETURN
C
C Set up the next pass in the other direction.
C
        ISTP=-ISTP
        ITMP=IBEG
        IBEG=IEND+ISTP
        IEND=ITMP
C
  102 CONTINUE
C
C Done.
C
      RETURN
C
      END
