C
C	$Id: ezmy.f,v 1.3 1992-09-04 20:33:14 ncargd Exp $
C
C
C ---------------------------------------------------------------------
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
