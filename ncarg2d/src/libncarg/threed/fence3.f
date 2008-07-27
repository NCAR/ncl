C
C $Id: fence3.f,v 1.6 2008-07-27 00:17:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FENCE3 (U,V,W,N,IOR,BOT)
      SAVE
      REAL            U(N)       ,V(N)       ,W(N)
      DIMENSION       LASF(13)
C
C COMMON BLOCK THRINT IS USED FOR SETTING COLOR INTENSITY
C
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
C
C INQUIRE LINE COLOR INDEX AND SET ASF TO INDIVIDUAL
C
      CALL GQPLCI (IERR, IPLCI)
      CALL GQASF (IERR, LASF)
      LSV3  = LASF(3)
      LASF(3) = 1
      CALL GSASF (LASF)
C
      M = N
      BASE = BOT
      L = MAX(1,MIN(3,IOR))
C
C SET LINE INTENSITY TO LOW
C
      CALL GSPLCI (ITHRMN)
      GO TO ( 10, 40, 70),L
   10 CALL FRST3 (BASE,V(1),W(1))
      DO  20 I=2,M
         VV = V(I)
         WW = W(I)
         CALL VECT3 (BASE,VV,WW)
   20 CONTINUE
      DO  30 I=1,M
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL LINE3 (UU,VV,WW,BASE,VV,WW)
   30 CONTINUE
      GO TO 100
   40 CALL FRST3 (U(1),BASE,W(1))
      DO  50 I=2,M
         UU = U(I)
         WW = W(I)
         CALL VECT3 (UU,BASE,WW)
   50 CONTINUE
      DO  60 I=1,M
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL LINE3 (UU,VV,WW,UU,BASE,WW)
   60 CONTINUE
      GO TO 100
   70 CALL FRST3 (U(1),V(1),BASE)
      DO  80 I=2,M
         UU = U(I)
         VV = V(I)
         CALL VECT3 (UU,VV,BASE)
   80 CONTINUE
      DO  90 I=1,M
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL LINE3 (UU,VV,WW,UU,VV,BASE)
   90 CONTINUE
C
C SET LINE INTENSITY TO HIGH
C
  100 CALL GSPLCI (ITHRMJ)
      CALL CURVE3 (U,V,W,M)
C
C RESTORE ASF AND LINE INTENSITY TO ORIGINAL
C
      LASF(3) = LSV3
      CALL GSASF (LASF)
      CALL GSPLCI (IPLCI)
C
      RETURN
C
C REVISION HISTORY---
C
C JANUARY 1978     DELETED REFERENCES TO THE  *COSY  CARDS AND
C                  ADDED REVISION HISTORY
C FEBURARY 1979    MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C JUNE 1979        UPDATED FILE TO INCLUDE BLOCK DATA PWRZBD AND
C                  CORRECT A COMMENTED OUT STATEMENT IN CURVE3.
C MARCH 1980       REMOVED THE PWRZ AND PWRITZ ENTRIES.  THESE
C                  CAPABILITIES WERE REPLACED WITH THE NEW ULIB FILE
C                  PWRZT.
C JULY 1984        CONVERTED TO FORTRAN 77 AND GKS
C MARCH 1990       CORRECTED THE SAVING AND RESTORING OF SET CALLS
C JULY 1990        CHANGED THE NAME OF THE COMMON BLOCK TEMPR TO
C                  TEMPRT (TO AVOID CONFLICT WITH ISOSRF).
C
C-----------------------------------------------------------------------
C
      END
