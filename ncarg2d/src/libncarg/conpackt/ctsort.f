C
C $Id: ctsort.f,v 1.2 2004-03-19 22:51:58 kennison Exp $
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
      SUBROUTINE CTSORT (RVAL,NVAL,IPER)
C
      DIMENSION RVAL(NVAL),IPER(NVAL)
C
C Given an array of NVAL reals in an array RVAL, this routine returns a
C permutation vector IPER such that, given I and J, 1.LE.I.LE.J.LE.NVAL,
C RVAL(IPER(I)).LE.RVAL(IPER(J)).
C
C A Shell sort is used.  Details of the algorithm may be found in the
C book "Algorithms" by Robert Sedgewick.
C
C Note:  Fred Clare wrote the original version of this routine.  I have
C adapted it for use in CONPACKT; among other things, the error checking
C has been been removed because the calling routine does it.  (DJK)
C
      DO 10001 I=1,NVAL
        IPER(I)=I
10001 CONTINUE
C
      K=0
C
10002 CONTINUE
      IF (.NOT.(3*K+1.LT.NVAL)) GO TO 10003
        K=3*K+1
      GO TO 10002
10003 CONTINUE
C
10004 CONTINUE
      IF (.NOT.(K.GT.0)) GO TO 10005
C
        DO 10006 I=1,NVAL-K
C
          J=I
C
10007     CONTINUE
            IF (RVAL(IPER(J)).LE.RVAL(IPER(J+K))) GO TO 10008
            ITMP=IPER(J)
            IPER(J)=IPER(J+K)
            IPER(J+K)=ITMP
            J=J-K
            IF (J.LT.1) GO TO 10008
          GO TO 10007
10008     CONTINUE
C
10006   CONTINUE
C
        K=(K-1)/3
C
      GO TO 10004
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
