C
C $Id: cthls2.f,v 1.1 2003-05-28 15:44:30 kennison Exp $
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
      SUBROUTINE CTHLS2 (RWRK,NRWK,IWRK)
C
      DIMENSION RWRK(NRWK),IWRK(NRWK)
C
C This is a modified version of a sort routine from TDPACK.  I think the
C original came from Fred Clare.
C
C Given an array of NRWK reals in an array RWRK, this routine returns
C a permutation vector IWRK such that, for every I and J such that
C 1.LE.I.LE.J.LE.NRWK, then RWRK(IWRK(I)).LE.RWRK(IWRK(J)).
C
      DO 101 I=1,NRWK
      IWRK(I)=I-1
  101 CONTINUE
C
      K=0
C
  102 IF (3*K+1.LT.NRWK) THEN
        K=3*K+1
        GO TO 102
      END IF
C
  103 IF (K.GT.0) THEN
C
        DO 105 I=1,NRWK-K
C
        J=I
C
  104   IF (RWRK(IWRK(J)+1).LE.RWRK(IWRK(J+K)+1)) GO TO 105
        ITMP=IWRK(J)
        IWRK(J)=IWRK(J+K)
        IWRK(J+K)=ITMP
        J=J-K
        IF (J.LT.1) GO TO 105
        GO TO 104
C
  105   CONTINUE
C
        K=(K-1)/3
C
        GO TO 103
C
      END IF
C
C Done.
C
      RETURN
C
      END
