C
C $Id: setusv.f,v 1.8 2008-07-27 00:17:25 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SETUSV (VN,IV)
      CHARACTER*(*) VN
C
C This subroutine sets the values of various utility state variables.
C VN is the name of the variable and IV is its value.
C
C The labelled common block IUTLCM contains all of the utility state
C variables.
C
      COMMON /IUTLCM/ IU(100)
      SAVE /IUTLCM/
C
C Define an array in which to get the GKS aspect source flags.
C
      DIMENSION LF(13)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SETUSV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for the linear-log scaling variable, which can take on these
C values:
C
C     1 = X linear, Y linear
C     2 = X linear, Y log
C     3 = X log   , Y linear
C     4 = X log   , Y log
C
      IF (VN(1:2).EQ.'LS') THEN
        IF (IV.LT.1.OR.IV.GT.4) THEN
          CALL SETER ('SETUSV - LOG SCALE VALUE OUT OF RANGE',2,1)
          RETURN
        END IF
        IU(1)=IV
C
C Check for the mirror-imaging variable, which can take on these
C values:
C
C     1 = X normal  , Y normal
C     2 = X normal  , Y reversed
C     3 = X reversed, Y normal
C     4 = X reversed, Y reversed
C
      ELSE IF (VN(1:2).EQ.'MI') THEN
        IF (IV.LT.1.OR.IV.GT.4) THEN
          CALL SETER ('SETUSV - MIRROR-IMAGING VALUE OUT OF RANGE',3,1)
          RETURN
        END IF
        IU(2)=IV
C
C Check for the scale factor setting the resolution of the plotter in
C the x direction.
C
      ELSE IF (VN(1:2).EQ.'XF') THEN
        IF (IV.LT.1.OR.IV.GT.15) THEN
          CALL SETER ('SETUSV - X RESOLUTION OUT OF RANGE',4,1)
          RETURN
        END IF
        IU(3)=IV
C
C Check for the scale factor setting the resolution of the plotter in
C the y direction.
C
      ELSE IF (VN(1:2).EQ.'YF') THEN
        IF (IV.LT.1.OR.IV.GT.15) THEN
          CALL SETER ('SETUSV - Y RESOLUTION OUT OF RANGE',5,1)
          RETURN
        END IF
        IU(4)=IV
C
C Check for the variable specifying the size of the pen-move buffer.
C
      ELSE IF (VN(1:2).EQ.'PB') THEN
        IF (IV.LT.2.OR.IV.GT.50) THEN
          CALL SETER ('SETUSV - PEN-MOVE BUFFER SIZE OUT OF RANGE',6,1)
          RETURN
        END IF
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('SETUSV',7).NE.0) RETURN
        IU(5)=IV
C
C Check for a metacode unit number.
C
      ELSE IF (VN(1:2).EQ.'MU') THEN
        IF (IV.LE.0) THEN
          CALL SETER ('SETUSV - METACODE UNIT NUMBER ILLEGAL',8,1)
          RETURN
        END IF
C
C What is done depends on whether OPNGKS has been called or not.  In
C either case, we end up with a new metacode unit.
C
        IF (IU(6).LT.0) THEN
          CALL GDAWK (1)
          CALL GCLWK (1)
          CALL GOPWK (1,IV,1)
          CALL GACWK (1)
          IU(6)=-IV
        ELSE
          IU(6)=IV
        END IF
C
C Check for one of the variables setting color and intensity.
C
      ELSE IF (VN(1:2).EQ.'IR') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF RED INTENSITY',9,1)
          RETURN
        END IF
        IU(7)=IV
C
      ELSE IF (VN(1:2).EQ.'IG') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF GREEN INTENSITY',10,1)
          RETURN
        END IF
        IU(8)=IV
C
      ELSE IF (VN(1:2).EQ.'IB') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF BLUE INTENSITY',11,1)
          RETURN
        END IF
        IU(9)=IV
C
      ELSE IF (VN(1:2).EQ.'IN') THEN
        IF (IV.LT.0.OR.IV.GT.10000) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF INTENSITY',12,1)
          RETURN
        END IF
        IU(10)=IV
C
C Assign the intensity-controlling variables to local variables with
C simple, meaningful names.
C
        IR=IU(7)
        IG=IU(8)
        IB=IU(9)
        IN=IU(10)
        II=IU(11)
        IM=IU(12)
C
C Compute the floating-point red, green, and blue intensities.
C
        FR=REAL(IR)/REAL(MAX(IR,IG,IB,1))*REAL(IN)/10000.
        FG=REAL(IG)/REAL(MAX(IR,IG,IB,1))*REAL(IN)/10000.
        FB=REAL(IB)/REAL(MAX(IR,IG,IB,1))*REAL(IN)/10000.
C
C Dump the pen-move buffer before changing anything.
C
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('SETUSV',13).NE.0) RETURN
C
C Set the aspect source flags for all the color indices to "individual".
C
        CALL GQASF (IE,LF)
        IF (IE.NE.0) THEN
          CALL SETER ('SETUSV - ERROR EXIT FROM GQASF',14,1)
          RETURN
        END IF
        LF( 3)=1
        LF( 6)=1
        LF(10)=1
        LF(13)=1
        CALL GSASF (LF)
C
C Pick a new color index and use it for polylines, polymarkers, text,
C and areas.
C
        II=MOD(II,IM)+1
        IU(11)=II
        CALL GSPLCI (II)
        CALL GSPMCI (II)
        CALL GSTXCI (II)
        CALL GSFACI (II)
C
C Now, redefine the color for that color index on each open workstation.
C
        CALL GQOPWK (0,IE,NO,ID)
        IF (IE.NE.0) THEN
          CALL SETER ('SETUSV - ERROR EXIT FROM GQOPWK',15,1)
          RETURN
        END IF
C
        DO 103 I=1,NO
          CALL GQOPWK (I,IE,NO,ID)
          IF (IE.NE.0) THEN
            CALL SETER ('SETUSV - ERROR EXIT FROM GQOPWK',16,1)
            RETURN
          END IF
          CALL GSCR (ID,II,FR,FG,FB)
  103   CONTINUE
C
C Check for variable resetting the color index.
C
      ELSE IF (VN(1:2).EQ.'II') THEN
C       IF (IV.LT.1.OR.IV.GT.IU(12)) THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL COLOR INDEX',17,1)
          RETURN
        END IF
        IU(11)=IV
C
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('SETUSV',18).NE.0) RETURN
C
        CALL GQASF (IE,LF)
        IF (IE.NE.0) THEN
          CALL SETER ('SETUSV - ERROR EXIT FROM GQASF',19,1)
          RETURN
        END IF
        LF( 3)=1
        LF( 6)=1
        LF(10)=1
        LF(13)=1
        CALL GSASF (LF)
C
        CALL GSPLCI (IV)
        CALL GSPMCI (IV)
        CALL GSTXCI (IV)
        CALL GSFACI (IV)
C
C Check for the variable limiting the values of color index used.
C
      ELSE IF (VN(1:2).EQ.'IM') THEN
        IF (IV.LT.1) THEN
          CALL SETER ('SETUSV - ILLEGAL MAXIMUM COLOR INDEX',20,1)
          RETURN
        END IF
        IU(12)=IV
C
C Check for the variable setting the current line width scale factor.
C
      ELSE IF (VN(1:2).EQ.'LW') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL LINE WIDTH SCALE FACTOR',21,1)
          RETURN
        END IF
        IU(13)=IV
C
C Dump the pen-move buffer before changing anything.
C
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('SETUSV',22).NE.0) RETURN
C
C Set the aspect source flag for linewidth scale factor to "individual".
C
        CALL GQASF (IE,LF)
        IF (IE.NE.0) THEN
          CALL SETER ('SETUSV - ERROR EXIT FROM GQASF',23,1)
          RETURN
        END IF
        LF(2)=1
        CALL GSASF (LF)
C
C Redefine the line width scale factor.
C
        CALL GSLWSC (REAL(IV)/1000.)
C
C Check for the variable setting the current marker size scale factor.
C
      ELSE IF (VN(1:2).EQ.'MS') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL MARKER SIZE SCALE FACTOR',24,1)
          RETURN
        END IF
        IU(14)=IV
C
C Set aspect source flag for marker size scale factor to "individual".
C
        CALL GQASF (IE,LF)
        IF (IE.NE.0) THEN
          CALL SETER ('SETUSV - ERROR EXIT FROM GQASF',25,1)
          RETURN
        END IF
        LF(5)=1
        CALL GSASF (LF)
C
C Redefine the marker size scale factor.
C
        CALL GSMKSC (REAL(IV)/1000.)
C
C Otherwise, the variable name is unknown.
C
      ELSE
        CALL SETER ('SETUSV - UNKNOWN VARIABLE NAME IN CALL',26,1)
        RETURN
C
      ENDIF
C
      RETURN
C
      END
