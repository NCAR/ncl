C
C ---------------------------------------------------------------------
C
      SUBROUTINE IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA)
C
      REAL XDRA(*),YDRA(*)
C
      INTEGER LDSH(*)
C
      CHARACTER*(*) LABX,LABY,LABG
C
      CHARACTER*16 AGBNCH
C
C This is an implementation of the routine from which AUTOGRAPH grew.
C It should work pretty much as the original did (if you can figure out
C what that was).
C
      CALL ANOTAT (LABX,LABY,1,2-ISIGN(1,NPTS),1,AGBNCH(LDSH))
C
      CALL DISPLA (2-MAX0(-1,MIN0(1,LFRA)),1,LTYP)
C
      CALL AGEZSU (5,XDRA,YDRA,IABS(NPTS),1,IABS(NPTS),LABG,IIVX,IIEX,
     +                                                        IIVY,IIEY)
      CALL AGBACK
C
      CALL AGCURV (XDRA,1,YDRA,1,IABS(NPTS),1)
C
      IF (LFRA.GT.0) CALL FRAME
C
      RETURN
C
      END
