C NCLFORTSTART
      SUBROUTINE QU2REG2(PFIELD,KPOINT,KLAT,KLON,KCODE,PMSVAL,KRET)
C
C**** QU2REG - Convert quasi-regular grid data to regular.
C
C     Purpose.
C     --------
C
C     Convert quasi-regular grid data to regular,
C     using either a linear or cubic interpolation.
C
C
C**   Interface.
C     ----------
C
C     CALL QU2REG2(PFIELD,KPOINT,KLAT,KLON,KCODE,PMSVAL)
C
C
C     Input Parameters.
C     -----------------
C
C     PFIELD     - Array containing quasi-regular grid
C                  data.
C
C     KPOINT     - Array containing list of the number of
C                  points on each latitude (or longitude) of
C                  the quasi-regular grid.
C
C     KLAT       - Number of latitude lines
C
C     KLON       - Number of longitude lines
C
C     KCODE      - Interpolation required.
C                  1 , linear - data quasi-regular on
C                               latitude lines.
C                  3 , cubic -  data quasi-regular on
C                               latitude lines.
C                  11, linear - data quasi-regular on
C                               longitude lines.
C                  13, cubic -  data quasi-regular on
C                               longitude lines.
C
C     PMSVAL     - Value used for missing data indicator.
C
C
C     Output Parameters.
C     ------------------
C
C     KRET       - return code
C                  0 = OK
C                  non-zero indicates fatal error
C
C
C     Output Parameters.
C     ------------------
C
C     PFIELD     - Array containing regular grid data.
C
C
C     Method.
C     -------
C
C     Data is interpolated and expanded into a temporary array,
C     which is then copied back into the user's array.
C     Returns an error code if an invalid interpolation is requested
C     or field size exceeds array dimensions.
C
C
C     Externals.
C     ----------
C
C     ROWINA2
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB code specifications of
C     quasi-regular grids.
C
C
C     Comments.
C     ---------
C
C     This routine is an adaptation of QU2REG to allow missing data
C     values, and hence bit mapped fields.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      22.07.94
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers     ECMWF      13.09.94
C     Add return code KRET and remove calls to ABORT.
C
C
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables. Data statements.
C     -----------------------------------------------------------------
C
c ncl IMPLICIT NONE   ! NCL does not support
C
      INTEGER ICODE
      INTEGER ILII
      INTEGER ILIO
      INTEGER IQUANO
      INTEGER IREGNO
C
      INTEGER JPMAX
C
      INTEGER J210
      INTEGER J220
      INTEGER J225
      INTEGER J230
      INTEGER J240
C
      INTEGER KCODE
      INTEGER KLAT
      INTEGER KLON
      INTEGER KPOINT
      INTEGER KRET
C
      REAL    PFIELD
      REAL    PMSVAL
C
      REAL    ZLINE
      REAL    ZTEMP
      REAL    ZWORK
C
C     Maximum number of latitudes (or longitudes), for which arrays
C     are dimensioned.
C
      DIMENSION PFIELD(*)
C
      DIMENSION KPOINT(*)
C NCLEND
c ncl In the original code the following 4 decalrations were after
c     the "DIMENSION PFIELD(*)" but they were moved here. 
      PARAMETER (JPMAX=320)
      DIMENSION ZTEMP(JPMAX*JPMAX*2)
      DIMENSION ZLINE(JPMAX*2)
      DIMENSION ZWORK(0:JPMAX*2+2,3)
C
C     ------------------------------------------------------------------
C*    Section 1. Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      KRET = 0
C
C     Check input parameters.
C
      IF (KCODE.NE.1.AND.KCODE.NE.3.AND.KCODE.NE.11.AND.KCODE.NE.13)
     X  THEN
	WRITE (*,9001) KCODE
        KRET = 1
        GOTO 900
      ENDIF
C
      IF (KLAT.GT.JPMAX) THEN
	WRITE (*,9002) KLAT , JPMAX
        KRET = 2
        GOTO 900
      ENDIF
C
      IF (KLON.GT.JPMAX*2) THEN
	WRITE (*,9003) KLAT , JPMAX*2
        KRET = 3
        GOTO 900
      ENDIF
C
C     Set array indices to 0.
C
      ILII  = 0
      ILIO  = 0
C
C     Establish values of loop parameters.
C
      IF (KCODE.GT.10) THEN
C
C       Quasi-regular along longitude lines.
C
	IQUANO = KLON
	IREGNO = KLAT
	ICODE  = KCODE - 10
      ELSE
C
C       Quasi-regular along latitude lines.
C
	IQUANO = KLAT
	IREGNO = KLON
	ICODE  = KCODE
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 2. Interpolate field from quasi to regular grid.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      DO 230 J230=1,IQUANO
C
        IF (IREGNO.NE.KPOINT(J230)) THEN
C
C         Line contains less values than required,so
C         extract quasi-regular grid values for a line
C
          DO 210 J210=1,KPOINT(J230)
            ILII        = ILII+1
            ZLINE(J210) = PFIELD(ILII)
  210     CONTINUE
C
C         and interpolate this line.
C
          CALL ROWINA2( ZLINE, IREGNO, KPOINT(J230), ZWORK, ICODE,
     X                  PMSVAL, KRET)
          IF ( KRET .NE. 0 ) GOTO 900
C
C         Add regular grid values for this line to the temporary array.
C
          DO 220 J220=1,IREGNO
            ILIO        = ILIO+1
            ZTEMP(ILIO) = ZLINE(J220)
  220     CONTINUE
C
        ELSE
C
C         Line contains the required number of values, so add
C         this line to the temporary array.
C
          DO 225 J225=1,IREGNO
            ILIO        = ILIO+1
            ILII        = ILII+1
            ZTEMP(ILIO) = PFIELD(ILII)
  225     CONTINUE
C
        ENDIF
C
  230 CONTINUE
C
C     Copy temporary array to user array.
C
      DO 240 J240=1,KLON*KLAT
	PFIELD(J240) = ZTEMP(J240)
  240 CONTINUE
C
C     ------------------------------------------------------------------
C*    Section 9. Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
 9001 FORMAT (1H ,'QU2REG : Invalid interpolation type code = ',I3)
C
 9002 FORMAT (1H ,'QU2REG : Number of latitudes is ',I4,', maximum ',
     C                      'allowed is ',I3,'.')
C
 9003 FORMAT (1H ,'QU2REG : Number of longitudes is ',I4,', maximum ',
     C                      'allowed is ',I3,'.')
C
      END

      SUBROUTINE ROWINA2( P, KO, KI, PW, KCODE, PMSVAL, KRET)
C
C**** ROWINA2 - Interpolation of row of values.
C
C     Purpose.
C     --------
C
C     Interpolate a row of values.
C
C
C**   Interface.
C     ----------
C
C     CALL ROWINA2( P, KO, KI, PW, KCODE, PMSVAL, KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     P      - Row of values to be interpolated.
C              Dimension must be at least KO.
C
C     KO     - Number of values required.
C
C     KI     - Number of values in P on input.
C
C     PW     - Working array.
C              Dimension must be at least (0:KO+2,3).
C
C     KCODE  - Interpolation required.
C              1 , linear.
C              3 , cubic.
C
C     PMSVAL - Value used for missing data indicator.
C
C
C     Output Parameters.
C     ------------------
C
C     P     - Now contains KO values.
C     KRET  - Return code
C             0, OK
C             Non-zero, error
C
C
C     Method.
C     -------
C
C     Linear or cubic interpolation performed as required.
C
C
C     Externals.
C     ----------
C
C     SCM0
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     ---------
C
C     This is a version of ROWINA which allows for missing data
C     values and hence for bitmapped fields.
C
C
C     Author.
C     -------
C
C     J.D.Chambers    ECMWF     22.07.94
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers    ECMWF     13.09.94
C     Add return code KRET and remove calls to ABORT.
C
C
C     -----------------------------------------------------------------
C
C*******************************************************************
C     Section 0.  Declarations.
C*******************************************************************
C
      IMPLICIT NONE
C
C     Subroutine arguments
      REAL P, PW, PMSVAL
      INTEGER KO,KI,KCODE, KRET
      DIMENSION P(KO),PW(0:KO+2,3)
C
C     Local variables
      INTEGER JL, IP
      REAL ZRDI, ZDO, ZPOS, ZWT, ZWT1
C
C*******************************************************************
C     Section 1.  Linear interpolation ..
C*******************************************************************
C
  100 CONTINUE
C
      KRET = 0
C
      IF( KCODE.EQ.1 ) THEN
C
C        Move input values to work array
         DO 110 JL = 1, KI
            PW(JL,1) = P(JL)
  110    CONTINUE
C
C        Arrange wrap-around value in work array
         PW(KI+1,1) = P(1)
C
C        Set up constants to be used to figure out weighting for
C        values in interpolation.
         ZRDI = FLOAT(KI)
         ZDO  = 1.0 / FLOAT(KO)
C
C        Loop through the output points
         DO 120 JL = 1, KO
C
C           Calculate weight from the start of row
            ZPOS = (JL-1) * ZDO
            ZWT  = ZPOS * ZRDI
C
C           Get the current array position(minus 1) from the weight -
C           note the implicit truncation.
            IP   = ZWT
C
C           If the left value is missing, use the right value
            IF ( PW(IP+1,1) .EQ. PMSVAL ) THEN
               P(JL) = PW(IP+2,1)
C
C           If the right value is missing, use the left value
            ELSE IF ( PW(IP+2,1) .EQ. PMSVAL ) THEN
               P(JL) = PW(IP+1,1)
C
C           If neither missing, interpolate ...
            ELSE
C
C             Adjust the weight to range (0.0 to 1.0)
              ZWT  = ZWT - IP
C
C             Interpolate using the weighted values on either side
C             of the output point position
              P(JL) = (1.0-ZWT) * PW(IP+1,1) + ZWT * PW(IP+2,1)
            ENDIF
C
  120    CONTINUE
C
C*******************************************************************
C     Section 2.  Cubic interpolation ..
C*******************************************************************
C
  200 CONTINUE
C
      ELSEIF(KCODE.EQ.3) THEN
         DO 210 JL = 1,KI
            IF ( P(JL) .EQ. PMSVAL ) THEN
              WRITE(*,*) ' ROWINA2: Cubic interpolation not supported'
              WRITE(*,*) ' ROWINA2: for fields containing missing data.'
              WRITE(*,*) ' Sorry!'
	      KRET = 1
              GOTO 900
            ENDIF
            PW(JL,1) = P(JL)
  210    CONTINUE
         PW(0,1) = P(KI)
         PW(KI+1,1) = P(1)
         PW(KI+2,1) = P(2)
         DO 220 JL = 1,KI
            PW(JL,2) =  - PW(JL-1,1)/3.0 - 0.5*PW(JL,1)
     1             + PW(JL+1,1)    - PW(JL+2,1)/6.0
            PW(JL+1,3) =    PW(JL-1,1)/6.0 - PW(JL,1)
     1             + 0.5*PW(JL+1,1) + PW(JL+2,1)/3.0
  220    CONTINUE
         CALL SCM0(PW(1,2),PW(2,3),PW(1,1),PW(2,1),KI)
         ZRDI = FLOAT(KI)
         ZDO = 1.0/FLOAT(KO)
         DO 230 JL = 1,KO
            ZPOS = (JL-1)*ZDO
            ZWT = ZPOS*ZRDI
            IP = ZWT+1
            ZWT = ZWT+1.0-IP
            ZWT1  =  1.0 - ZWT
            P(JL) = ((3.0-2.0*ZWT1)*PW(IP,1) + ZWT*PW(IP,2))*ZWT1*ZWT1
     1       + ((3.0-2.0*ZWT) *PW(IP+1,1) - ZWT1*PW(IP+1,3))*ZWT*ZWT
  230    CONTINUE
C
      ELSE
C
C*******************************************************************
C     Section 3.  Invalid interpolation code ..
C*******************************************************************
C
  300   CONTINUE
C
        WRITE (*,9001) KCODE
        KRET = 2
      ENDIF
C
C*******************************************************************
C     Section 9.  Closedown.
C*******************************************************************
C
900   CONTINUE
      RETURN
C
 9001 FORMAT (1H ,'ROWINA2 : Invalid interpolation code = ',I4)
C
      END
      SUBROUTINE SCM0   (PDL,PDR,PFL,PFR,KLG)
C
C**** SCM0   - Apply SCM0 limiter to derivative estimates.
C
C    M. HORTAL    ECMWF February 1991  closely following D. WILLIAMSON
C
C    Apply SCM0 limiter to derivative estimates.
C
C   output:
C     pdl   = the limited derivative at the left edge of the interval
C     pdr   = the limited derivative at the right edge of the interval
C
C   inputs
C     pdl   = the original derivative at the left edge
C     pdr   = the original derivative at the right edge
C     pfl   = function value at the left edge of the interval
C     pfr   = function value at the right edge of the interval
C     klg  = number of intervals where the derivatives are limited
C
      DIMENSION PDL(KLG),PDR(KLG),PFL(KLG),PFR(KLG)
  100 CONTINUE
C
C    define constants
C
      ZEPS=1.E-12
      ZFAC=3.*(1.-ZEPS)
C
      DO 200 JL=1,KLG
      IF(ABS(PFR(JL)-PFL(JL)).GT.ZEPS) THEN
          ZALPHA=PDL(JL)/(PFR(JL)-PFL(JL))
          ZBETA =PDR(JL)/(PFR(JL)-PFL(JL))
          IF(ZALPHA.LE.0.) PDL(JL)=0.
          IF(ZBETA .LE.0.) PDR(JL)=0.
          IF(ZALPHA.GT.ZFAC) PDL(JL)=ZFAC*(PFR(JL)-PFL(JL))
          IF(ZBETA .GT.ZFAC) PDR(JL)=ZFAC*(PFR(JL)-PFL(JL))
      ELSE
          PDL(JL)=0.
          PDR(JL)=0.
      ENDIF
  200 CONTINUE
      END
