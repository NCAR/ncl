C
C $Id: cphlso.f,v 1.1 2002-01-29 22:48:39 kennison Exp $
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
      SUBROUTINE CPHLSO (RWRK,IDRW,MRWK,NRWK,IWRK)
C
      DIMENSION RWRK(IDRW,*),IWRK(*)
C
C This is a modification of a sort routine from TDPACK.  I think the
C original came from Fred Clare.
C
C Given MRWK*NRWK reals in an array RWRK (whose first dimension is IDRW)
C and an integer array IWRK of length MRWK*NRWK, CPHLSO returns in IWRK
C an array of indices of the elements of RWRK such that, if M and N are
C in [1,MRWK*NRWK] and M.LE.N, then the element of RWRK indexed by the
C Mth element of IWRK is less than or equal to the one indexed by the
C Nth element of IWRK.
C
C RVAL is an arithmetic statement function which, given an integer
C index I between 0 and MRWK*NRWK-1, has as its value the Ith element
C (in column-wise order) of the array RWRK.
C
      RVAL(I)=RWRK(MOD(I,MRWK)+1,I/MRWK+1)
C
C If the first dimension of the array of data to be sorted is equal to
C the first dimension of the FORTRAN array in which it is stored, use
C a simpler, faster version of the code to process it.
C
      IF (IDRW.EQ.MRWK) THEN
        CALL CPHLS2 (RWRK,MRWK*NRWK,IWRK)
C
C Otherwise ...
C
      ELSE
C
C ... generate indices 0 through MRWK*NRWK-1 in the array IWRK and ...
C
        DO 101 I=1,MRWK*NRWK
        IWRK(I)=I-1
  101   CONTINUE
C
C ... sort them.
C
        K=0
C
  102   IF (3*K+1.LT.MRWK*NRWK) THEN
          K=3*K+1
          GO TO 102
        END IF
C
  103   IF (K.GT.0) THEN
C
          DO 105 I=1,MRWK*NRWK-K
C
          J=I
C
  104     IF (RVAL(IWRK(J)).LE.RVAL(IWRK(J+K))) GO TO 105
          ITMP=IWRK(J)
          IWRK(J)=IWRK(J+K)
          IWRK(J+K)=ITMP
          J=J-K
          IF (J.LT.1) GO TO 105
          GO TO 104
C
  105     CONTINUE
C
          K=(K-1)/3
C
          GO TO 103
C
        END IF
C
      END IF
C
C Done.
C
      RETURN
C
      END
