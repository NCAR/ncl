C
C	$Id: ezmxy.f,v 1.1.1.1 1992-04-17 22:31:04 ncargd Exp $
C
C
C ---------------------------------------------------------------------
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
C Do statistics-gathering call.
C
      LOGICAL Q8Q4
      SAVE Q8Q4
      DATA Q8Q4 /.TRUE./
      IF (Q8Q4) THEN
        CALL Q8QST4('GRAPHX','AUTOGRAPH','EZMXY','VERSION 07')
        Q8Q4 = .FALSE.
      ENDIF
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
           KDSH=ISIGN(I,IDSH)
           CALL AGCURV (XDRA(INXD),IIEX,YDRA(INYD),IIEY,NPTS,KDSH)
  101      CONTINUE
C
  102 IF (IFRA.EQ.1) CALL FRAME
C
      RETURN
C
      END
