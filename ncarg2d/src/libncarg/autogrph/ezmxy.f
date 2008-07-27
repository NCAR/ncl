C
C $Id: ezmxy.f,v 1.8 2008-07-27 00:14:36 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE EZMXY (XDRA,YDRA,IDXY,MANY,NPTS,LABG)
C
      REAL XDRA(*),YDRA(*)
C
      CHARACTER*(*) LABG
C
C The routine EZMXY draws many curves, each of them defined by points of
C the form (XDRA(I,J),YDRA(I,J)) or (XDRA(J,I),YDRA(J,I)) or, possibly,
C (XDRA(I),YDRA(I,J)) or (XDRA(I),YDRA(J,I)), for I = 1, 2, ... NPTS and
C for J = 1, 2, ... MANY.  (YDRA is actually dimensioned IDXY by * .)
C
      CALL AGGETI ('SET .',ISET)
      CALL AGGETI ('FRAM.',IFRA)
      CALL AGGETI ('DASH/SELE.',IDSH)
C
      CALL AGEZSU (4,XDRA,YDRA,IDXY,MANY,NPTS,LABG,IIVX,IIEX,IIVY,IIEY)
      CALL AGBACK
C
      IF (ISET.LT.0) GO TO 102
C
           DO 101 I=1,MANY
           INXD=1+(I-1)*IIVX
           INYD=1+(I-1)*IIVY
           KDSH=SIGN(I,IDSH)
           CALL AGCURV (XDRA(INXD),IIEX,YDRA(INYD),IIEY,NPTS,KDSH)
  101      CONTINUE
C
  102 IF (IFRA.EQ.1) CALL FRAME
C
      RETURN
C
      END
