C
C $Id: ezmy.f,v 1.6 2000-08-22 15:02:19 haley Exp $
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
      SUBROUTINE EZMY (YDRA,IDXY,MANY,NPTS,LABG)
C
      REAL XDRA(1),YDRA(*)
C
      CHARACTER*(*) LABG
C
C The routine EZMY draws many curves, each of them defined by points of
C the form (I,YDRA(I,J)) or (I,YDRA(J,I)), for I = 1, 2, ... NPTS and
C for J = 1, 2, ... MANY.  (YDRA is actually dimensioned IDXY by * .)
C
      CALL AGGETI ('SET .',ISET)
      CALL AGGETI ('FRAM.',IFRA)
      CALL AGGETI ('DASH/SELE.',IDSH)
C
      CALL AGEZSU (3,XDRA,YDRA,IDXY,MANY,NPTS,LABG,IIVX,IIEX,IIVY,IIEY)
      CALL AGBACK
C
      IF (ISET.LT.0) GO TO 102
C
           DO 101 I=1,MANY
           INYD=1+(I-1)*IIVY
           KDSH=ISIGN(I,IDSH)
           CALL AGCURV (XDRA,0,YDRA(INYD),IIEY,NPTS,KDSH)
  101      CONTINUE
C
  102 IF (IFRA.EQ.1) CALL FRAME
C
      RETURN
C
      END
