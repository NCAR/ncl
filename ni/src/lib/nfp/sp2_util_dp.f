c orig code [ sp2_util.orig ]: John Adams
c .          check arguments to make sure calls are correct
c
      SUBROUTINE DGEOMATV(NLON,NLAT,U,V,WORK)
c
c pay special attention to subscript order
c .   nlon is first   [ u(nlon,nlat),v(nlon,nlat) ]
c
c     when invoked, this subroutine converts the nlon by nlat
c     geophysical vector field (u,v) to nlat by nlon mathematical
c     coordinates.  work is an UNSAVED work space of length at
c     least nlon*nlat
c
      IMPLICIT NONE
      INTEGER NLON,NLAT,I,J
      DOUBLE PRECISION U(NLON,NLAT),V(NLON,NLAT),WORK(*)
c
c     negate v
c
      DO J = 1,NLON
          DO I = 1,NLAT
              V(J,I) = -V(J,I)
          END DO
      END DO
c
c     traspose u,v
c
      CALL DTPLATX(NLON,NLAT,U,WORK)
      CALL DTPLATX(NLON,NLAT,V,WORK)
c
c     reverse (co)latitude subscript order
c
      CALL DCLATX(NLAT,NLON,U)
      CALL DCLATX(NLAT,NLON,V)

      RETURN
      END
c ---------------------------------------------------------
      SUBROUTINE DMATGEOV(NLAT,NLON,U,V,WORK)
c
c pay special attention to subscript order
c .   nlat is first dim  [ u(nlat,nlon), v(nlat,nlon) ]
c
c     when invoked, this subroutine converts the nlat by nlon
c     vector u & v, given in mathematical spherical coordinates, to
c     nlon by nlat vector field given in geophysical spherical
c     coordinates.  work is an UNSAVED work space of length at least
c     nlon*nlat
c
      IMPLICIT NONE
      INTEGER NLON,NLAT,I,J
      DOUBLE PRECISION U(NLAT,NLON),V(NLAT,NLON),WORK(*)
c
c     negate v
c
      DO J = 1,NLON
          DO I = 1,NLAT
              V(I,J) = -V(I,J)
          END DO
      END DO
c
c     reverse (co)latitude subscript order
c
      CALL DCLATX(NLAT,NLON,U)
      CALL DCLATX(NLAT,NLON,V)
c
c     transpose array
c
      CALL DTPLATX(NLAT,NLON,U,WORK)
      CALL DTPLATX(NLAT,NLON,V,WORK)

      RETURN
      END
c -------------------------------------------------------
      SUBROUTINE DGEOMAT(NLON,NLAT,S,WORK)
c
c pay special attention to subscript order
c .   nlon is first dim  [ s(nlon,nlat) ]
c
c     when invoked, this subroutine converts the nlon by nlat
c     scalar s, given in geophysical spherical coordinates, to
c     a nlat by nlon array given in mathematical spherical
c     coordinates.  work is an UNSAVED work space of length at
c     least nlon*nlat
c
      IMPLICIT NONE
      INTEGER NLON,NLAT
      DOUBLE PRECISION S(*),WORK(*)
c
c     traspose array
c
      CALL DTPLATX(NLON,NLAT,S,WORK)
c
c     reverse (co)latitude subscript order
c
      CALL DCLATX(NLAT,NLON,S)
      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DMATGEO(NLAT,NLON,S,WORK)
c
c pay special attention to subscript order
c .   nlat is first dim  [ s(nlat,nlon) ]
c
c     when invoked, this subroutine converts the nlat by nlon
c     array s, given in mathematical spherical coordinates, to
c     a nlon by nlat array given in geophysical spherical
c     coordinates.  work is an UNSAVED work space of length at least
c     nlon*nlat
c
      IMPLICIT NONE
      INTEGER NLON,NLAT
      DOUBLE PRECISION S(*),WORK(*)
c
c     reverse (co)latitude subscript order
c
      CALL DCLATX(NLAT,NLON,S)
c
c     transpose array
c
      CALL DTPLATX(NLAT,NLON,S,WORK)
      RETURN
      END
c -------------------------------------------------------------
      SUBROUTINE DTPLATX(N,M,DATA,WORK)
c
c     transpose the n by m array data to a m by n array data
c     work must be at least n*m words long
c
      IMPLICIT NONE
      INTEGER N,M,I,J,IJ,JI
      DOUBLE PRECISION DATA(*),WORK(*)

      DO J = 1,M
          DO I = 1,N
              IJ = (J-1)*N + I
              WORK(IJ) = DATA(IJ)
          END DO
      END DO

      DO I = 1,N
          DO J = 1,M
              JI = (I-1)*M + J
              IJ = (J-1)*N + I
              DATA(JI) = WORK(IJ)
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------
      SUBROUTINE DCLATX(NLAT,NLON,DATA)
c
c     reverse order of latitude (colatitude) grids
c
      IMPLICIT NONE
      INTEGER NLAT,NLON,NLAT2,I,IB,J
      DOUBLE PRECISION DATA(NLAT,NLON),TEMP

      NLAT2 = NLAT/2
      DO I = 1,NLAT2
          IB = NLAT - I + 1
          DO J = 1,NLON
              TEMP = DATA(I,J)
              DATA(I,J) = DATA(IB,J)
              DATA(IB,J) = TEMP
          END DO
      END DO

      RETURN
      END
c ------------------------------------
      SUBROUTINE DCHKERR(STRING1,STRING2,IER,JER,KER,MER)
      IMPLICIT NONE

      INTEGER IER,JER,KER,MER
      CHARACTER*(*) STRING1,STRING2

      RETURN

      IF (IER.NE.0 .OR. JER.NE.0 .OR. KER.NE.0 .OR. MER.NE.0) THEN
          WRITE (*,FMT=
     +'('' ERROR: '',a,'' : '',a
     + ,'' : ier,jer,ker,mer='',4i3)') STRING1,STRING2,IER,JER,KER,MER
          STOP
      END IF
      END
c ---------------------------------------------------------
      SUBROUTINE DTRCTPR(NLAT,MWAVE,MDAB,NDAB,NT,A,B,IER)
      IMPLICIT NONE

c truncate spectral coefficients
c .   (possibly) taper the spectral coef

      INTEGER NLAT,MWAVE,MDAB,NDAB,NT,IER
      DOUBLE PRECISION A(MDAB,NDAB,NT),B(MDAB,NDAB,NT)

      DOUBLE PRECISION CON
      INTEGER NTWGT,M,N,IWAVE,JP,JW,J,K,MW2
      PARAMETER (NTWGT=500)
      DOUBLE PRECISION TWGT(NTWGT)

      IER = 0
      IF (MWAVE.EQ.0) RETURN

      MW2 = IABS(MWAVE) + 2

      DO K = 1,NT
          DO N = MW2,NLAT
              DO M = 1,N
                  A(M,N,K) = 0.0D0
                  B(M,N,K) = 0.0D0
              END DO
          END DO
      END DO

      IF (MWAVE.GT.0) RETURN

      IWAVE = IABS(MWAVE)
C perform exponential tapering

C exponent; determines fall-off rate
      JP = MAX0(IWAVE/10,1)
C coef which has wgt=exp(-1)
      JW = JP*10

      IF ((IWAVE+1).GT.NTWGT) THEN
          WRITE (*,FMT='(''taprwv: ntwgt exceeded='',2i5)') NTWGT,
     +      (IWAVE+1)
          RETURN
      END IF

      CON = 1.D0/DBLE(JW* (JW+1))
      DO J = 0,IWAVE
          TWGT(J+1) = EXP(- (DBLE(J* (J+1))*CON)**JP)
c c c    write (*,'(''taprwv: j, twgt(j)='',2i5,1x,f15.7)')
c c c*                        j, (j+1),twgt(j+1)
      END DO
C now wgt the coef by the wgts

      DO K = 1,NT
C traverse the diagonal
          DO N = IWAVE + 1,1,-1
              DO M = 1,N
                  A(M,N,K) = A(M,N,K)*TWGT(N)
                  B(M,N,K) = B(M,N,K)*TWGT(N)
              END DO
          END DO
      END DO

      RETURN
      END
c ----------------------------------------------------
      SUBROUTINE DGEOSCL(MLON,NLAT,NT,X,SCALE,NER)
      IMPLICIT NONE
      INTEGER MLON,NLAT,NT,NER
      DOUBLE PRECISION X(MLON,NLAT,NT),SCALE

      INTEGER N,ML,NL

c scale an array

      NER = 0
      IF (SCALE.EQ.1.0D0) RETURN

      DO N = 1,NT
          DO NL = 1,NLAT
              DO ML = 1,MLON
                  X(ML,NL,N) = X(ML,NL,N)*SCALE
              END DO
          END DO
      END DO

      RETURN
      END
