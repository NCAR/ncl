c---------------------------------------------------------------------
c     subroutine trcwav(igridb,nlatb,nlonb,DB,wsave,lsave,work,lwork,
c    +                  dwork,ldwork,ier,JWAVE)
c
c *** author
c
c     John C, Adams (NCAR 1997), email: johnad@ncar.ucar.edu
c                                phone: 303-497-1213
c
c *** purpose
c
c     Given the scalar field DB on the sphere, subroutine trcwav
c     eliminates harmonic waves of DB with number greater than JWAVE.
c     So, for example, when transferring grid data from T63 to T42,
C*PL*ERROR* Comment line too long
c     trcwav can be called with JWAVE=42 after calling trssph to eliminate
c     frequencies higher than 42.
c
c *** the arguments igridb,nlatb,nlonb,wsave,lsave,work,lwork,ier
c     for trcwav are the same as the corresponding arguments in trssph.
C*PL*ERROR* Comment line too long
c     See the documentation of trssph for a description.  The last argument
C*PL*ERROR* Comment line too long
c     JWAVE sets the wave truncation. The work space lengths in lsave and
C*PL*ERROR* Comment line too long
c     lwork should be set as in trssph.  They are not checked in this code.
c
      SUBROUTINE DTRCWAV(IGRIDB,NLATB,NLONB,DB,WSAVE,LSAVE,WORK,LWORK,
     +                  DWORK,LDWORK,IER,JWAVE)
      IMPLICIT NONE
      INTEGER IGRIDB(2),NLONB,NLATB
      INTEGER LSAVE,LWORK,LDWORK,IER,JWAVE
      DOUBLE PRECISION DB(*),WSAVE(*),WORK(*)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)
      INTEGER IG,IGRDB,LB1,LB2,IAB,IBB,LW,IW,NT,ISYM
      SAVE

c
c     check input arguments
c
      IER = 6
      IG = IGRIDB(1)
      IF ((IG-1)* (IG+1)* (IG-2)* (IG+2).NE.0) RETURN
      IER = 7
      IG = IGRIDB(2)
      IF (IG* (IG-1).NE.0) RETURN
      IER = 8
      IF (NLONB.LT.4) RETURN
      IER = 9
      IF (NLATB.LT.3) RETURN
      IER = 0

      LB1 = MIN0(NLATB, (NLONB+2)/2)
      LB2 = (NLATB+1)/2
c
c     set pointers for spherical harmonic coefs
c
      IAB = 1
      IBB = IAB + LB1*NLATB
      IGRDB = IABS(IGRIDB(1))
c
c     set pointers for remaining work
c
      IW = IBB + LB1*NLATB
c
c     set remaining work space length in lw
c
      LW = LWORK - IW

      IF (IGRDB.EQ.2) THEN
c
c     initialize wsave for gaussian synthesis

          CALL DSHSGCI(NLATB,NLONB,WSAVE,LSAVE,DWORK,LDWORK,IER)
csp1  call shigc (nlatb,nlonb,wsave,lsave,dwork,ldwork,ier)
          IF (IER.NE.0) THEN
c
c     flag failure in spherepack gaussian software
c
              IER = 12
              RETURN
          END IF
      ELSE
c
c     initialize wsave for equally spaced synthesis
c
          CALL DSHSECI(NLATB,NLONB,WSAVE,LSAVE,DWORK,LDWORK,IER)
      END IF
c
c     transpose and/or reorder (co)latitude if necessary for DB
c     (arrays must have latitude (colatitude) as the first dimension
c     and run north to south for spherepack software)
c
      IF (IGRIDB(2).EQ.0) CALL DTRSPLAT(NLONB,NLATB,DB,WORK)
      IF (IGRIDB(1).GT.0) CALL DCONVLAT(NLATB,NLONB,DB)

      NT = 1
      ISYM = 0
      IF (IGRDB.EQ.2) THEN
c
c     do spherical harmonic analysis of "adjusted" DB on gaussian grid
c
          CALL DSHAGC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +               WORK(IBB),LB1,NLATB,WSAVE,LSAVE,WORK(IW),LW,IER)
      ELSE
c
C*PL*ERROR* Comment line too long
c     do spherical harmonic analysis of "adjusted" DB on equally spaced grid
c
          CALL DSHAEC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +               WORK(IBB),LB1,NLATB,WSAVE,LSAVE,WORK(IW),LW,IER)
      END IF
c
c     set DB wave number coefficients greater than JWAVE to zero
c
      CALL DTRCDB(LB1,NLATB,WORK(IAB),WORK(IBB),JWAVE,IER)
C
C     regenerate DB with truncated coefficients
c
      IF (IGRDB.EQ.1) THEN
c
C*PL*ERROR* Comment line too long
c     do spherical harmonic synthesis on nlatb by nlonb equally spaced grid
c
          CALL DSHSEC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +               WORK(IBB),LB1,NLATB,WSAVE,LSAVE,WORK(IW),LW,IER)
      ELSE
c
c     do spherical harmonic synthesis on nlatb by nlonb gaussian grid
c
          CALL DSHSGC(NLATB,NLONB,ISYM,NT,DB,NLATB,NLONB,WORK(IAB),
     +               WORK(IBB),LB1,NLATB,WSAVE,LSAVE,WORK(IW),LW,IER)
      END IF
c
c     DB is now latitude by longitude north to south array
c     restore DB to agree with flag igridb
c
      IF (IGRIDB(1).GT.0) CALL DCONVLAT(NLATB,NLONB,DB)
      IF (IGRIDB(2).EQ.0) CALL DTRSPLAT(NLATB,NLONB,DB,WORK)

      RETURN
      END
c -----------------------------------------------------
      SUBROUTINE DTRCDB(MB,NB,AB,BB,JWAVE,JER)
c
c     eliminate coefficients for wave numbers > JWAVE
c     triangular truncation at JWAVE
c
      IMPLICIT NONE
      INTEGER MB,NB,JWAVE,JER
      DOUBLE PRECISION AB(MB,NB),BB(MB,NB)

C local
      INTEGER J,M,N,IWAVE,JP,JW,KER,MER
C local
      DOUBLE PRECISION CON

c c c real, allocatable, dimension(:) :: twgt
      INTEGER NTWGT
      PARAMETER (NTWGT=500)
      DOUBLE PRECISION TWGT(NTWGT)

      JER = 0

      IWAVE = IABS(JWAVE)
C test for error
      IF ((IWAVE+2).GT.NB) THEN
          JER = -8
          WRITE (*,FMT=
     +'(''SUB TRCDB/TRCWAV: jwave too large: ''
     + ,''iwave,jwave,nb='',3i5)') IWAVE,JWAVE,NB
      ELSE IF (NB.NE.MB) THEN
          JER = -8
          WRITE (*,FMT=
     +'(''SUB TRCDB/TRCWAV: nb .ne. mb: ''
     + ,''nb,mb='',2i5)') NB,MB
      END IF
      IF (JER.NE.0) RETURN

C perform triangular truncation
      DO N = IWAVE + 2,NB
          DO M = 1,MB
              AB(M,N) = 0.0D0
              BB(M,N) = 0.0D0
          END DO
      END DO

C must *not* want to taper
      IF (JWAVE.GT.0) RETURN
C --------------------------

C perform exponential tapering

C exponent; determines fall-off rate
      JP = MAX0(IWAVE/10,1)
C coef which has wgt=exp(-1)
      JW = JP*10

c c c write (*,'(''trcdb: jp,jw,jwave,iwave,nb,mb='',6i5)')
c c c*                    jp,jw,jwave,iwave,nb,mb


c c c allocate (twgt(iwave+1), stat=ker)
      IF ((IWAVE+1).GT.NTWGT) THEN
          WRITE (*,FMT='(''trcdb: ntwgt exceeded='',2i5)') NTWGT,
     +      (IWAVE+1)
          RETURN
      END IF

      CON = 1.D0/DBLE(JW* (JW+1))
      DO J = 0,IWAVE
          TWGT(J+1) = EXP(- (DBLE(J* (J+1))*CON)**JP)
c c c    write (*,'(''trcdb: j, twgt(j)='',2i5,1x,f15.7)')
c c c*                       j, (j+1),twgt(j+1)
      END DO
C now wgt the coef by the wgts

C traverse the diagonal
      DO N = IWAVE + 1,1,-1
          DO M = 1,N
              AB(M,N) = AB(M,N)*TWGT(N)
              BB(M,N) = BB(M,N)*TWGT(N)
          END DO
      END DO

c c c deallocate (twgt, stat=mer)

      RETURN
      END
