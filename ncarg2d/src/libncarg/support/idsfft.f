C      SUBROUTINE IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,XI,YI,ZI,IWK,WK)
C
C DIMENSION OF           XD(NDP), YD(NDP),     ZD(NDP),   XI(NXI),
C ARGUMENTS              YI(NYI), ZI(NZI,NYI), WK(6*NDP),
C                        IWK(31*NDP + NXI*NYI)
C
C PURPOSE                THIS SUBROUTINE PERFORMS SMOOTH SURFACE
C                        FITTING WHEN THE PROJECTIONS OF THE DATA
C                        POINTS IN THE X-Y PLANE ARE IRREGULARLY
C                        DISTRIBUTED IN THE PLANE.
C
C USAGE                  CALL IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,
C                                    XI,YI,ZI,IWK,WK)
C
C ARGUMENTS
C
C ON INPUT               MD
C                          MODE OF COMPUTATION (MUST BE 1, 2, OR 3,
C                          ELSE AN ERROR RETURN WILL OCCUR).
C                           = 1 IF THIS IS THE FIRST CALL TO THIS
C                               SUBROUTINE, OR IF THE VALUE OF NDP
C                               HAS BEEN CHANGED FROM THE PREVIOUS
C                               CALL, OR IF THE CONTENTS OF THE XD
C                               OR YD ARRAYS HAVE BEEN CHANGED FROM
C                               THE PREVIOUS CALL.
C                           = 2 IF THE VALUES OF NDP AND THE XD,
C                               YD ARRAYS ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, BUT NEW VALUES FOR
C                               XI, YI ARE BEING USED.  IF MD = 2
C                               AND NDP HAS BEEN CHANGED SINCE THE
C                               PREVIOUS CALL TO IDSFFT, AN ERROR
C                               RETURN OCCURS.
C                           = 3 IF THE VALUES OF NDP, NXI, NYI, XD,
C                               YD, XI, YI ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, I.E. IF THE ONLY CHANGE
C                               ON INPUT TO IDSFFT IS IN THE ZD ARRAY.
C                               IF MD = 3 AND NDP, NXI OR NYI HAS BEEN
C                               CHANGED SINCE THE PREVIOUS CALL TO
C                               IDSFFT, AN ERROR RETURN OCCURS.
C
C                           BETWEEN THE CALL WITH MD=2 OR MD=3 AND
C                           THE PRECEDING CALL, THE IWK AND WK WORK
C                           ARRAYS SHOULD NOT BE DISTURBED.
C
C                        NDP
C                          NUMBER OF DATA POINTS (MUST BE 4 OR
C                          GREATER, ELSE AN ERROR RETURN WILL OCCUR).
C
C                        XD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE X
C                          COORDINATES OF THE DATA POINTS.
C
C                        YD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE Y
C                          COORDINATES OF THE DATA POINTS.
C
C                        ZD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE Z
C                          COORDINATES OF THE DATA POINTS.
C
C                        NXI
C                          NUMBER OF OUTPUT GRID POINTS IN THE X-
C                          DIRECTION (MUST BE 1 OR GREATER, ELSE
C                          AN ERROR RETURN WILL OCCUR).
C
C                        NYI
C                          NUMBER OF OUTPUT GRID POINTS IN THE Y-
C                          DIRECTION (MUST BE 1 OR GREATER, ELSE
C                          AN ERROR RETURN WILL OCCUR).
C
C                        NZI
C                          FIRST DIMENSION OF ZI AS DECLARED IN THE
C                          CALLING PROGRAM.  NZI MUST BE GREATER THAN
C                          OR EQUAL TO NXI, ELSE AN ERROR RETURN WILL
C                          OCCUR.
C
C                        XI
C                          ARRAY OF DIMENSION  NXI  CONTAINING THE
C                          X COORDINATES OF THE OUTPUT GRID POINTS.
C
C                        YI
C                         ARRAY OF DIMENSION  NYI  CONTAINING THE
C                         Y COORDINATES OF THE OUTPUT GRID POINTS.
C
C                        IWK
C                          INTEGER WORK ARRAY OF DIMENSION AT
C                          LEAST 31*NDP + NXI*NYI
C
C                        WK
C                          REAL WORK ARRAY OF DIMENSION AT LEAST 6*NDP
C
C ON OUTPUT              ZI
C                           REAL, TWO-DIMENSIONAL ARRAY OF DIMENSION
C                           (NZI,NYI), STORING THE INTERPOLATED Z
C                           VALUES AT THE OUTPUT GRID POINTS.
C
C SPECIAL CONDITIONS     INADEQUATE WORK SPACE IWK AND WK MAY
C                        MAY CAUSE INCORRECT RESULTS.
C
C                        THE DATA POINTS MUST BE DISTINCT AND THEIR
C                        PROJECTIONS IN THE X-Y PLANE MUST NOT BE
C                        COLLINEAR, OTHERWISE AN ERROR RETURN OCCURS.
C ********************************************************************
      SUBROUTINE IDSFFT (MD,NDP,XD,YD,ZD,NXI,NYI,NZI,XI,YI,ZI,
     1                   IWK,WK)
C THIS SUBROUTINE CALLS THE IDGRID, IDPDRV, IDPTIP, AND IDTANG
C SUBROUTINES.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP), YD(NDP),     ZD(NDP),               XI(NXI),
     1          YI(NYI), ZI(NZI,NYI), IWK(31*NDP + NXI*NYI), WK(6*NDP)
      COMMON/IDPT/ITPV,DMMY(27)
C
C  THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES.
C (FOR MD=1,2,3)
   10 MD0=MD
      NDP0=NDP
      NXI0=NXI
      NYI0=NYI
C ERROR CHECK.  (FOR MD=1,2,3)
   20 IF (MD0.LT.1.OR.MD0.GT.3) THEN
         CALL ULIBER (39,
     1' IDSFFT (BIVAR) - INPUT PARAMETER MD OUT OF RANGE',49)
         STOP 'ULIBER39'
      ENDIF
      IF (NDP0.LT.4) THEN
         CALL ULIBER (40,
     1' IDSFFT (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE',50)
         STOP 'ULIBER40'
      ENDIF
      IF (NXI0.LT.1.OR.NYI0.LT.1) THEN
         CALL ULIBER (41,
     1' IDSFFT (BIVAR) - INPUT PARAMETER NXI OR NYI OUT OF RANGE',57)
         STOP 'ULIBER41'
      ENDIF
      IF (NXI0.GT.NZI) THEN
         CALL ULIBER (42,
     1' IDSFFT (BIVAR) - INPUT PARAMETER NZI IS LESS THAN NXI',54)
         STOP 'ULIBER42'
      ENDIF
      IF(MD0.GT.1)        GO TO 21
      IWK(1)=NDP0
      GO TO 22
   21 NDPPV=IWK(1)
      IF (NDP0.NE.NDPPV) THEN
         CALL ULIBER (43,
     1' IDSFFT (BIVAR) - MD=2 OR 3 BUT NDP WAS CHANGED SINCE LAST CALL',
     2                63)
         STOP 'ULIBER43'
      ENDIF
   22 IF(MD0.GT.2)        GO TO 23
      IWK(3)=NXI0
      IWK(4)=NYI0
      GO TO 30
   23 NXIPV=IWK(3)
      NYIPV=IWK(4)
      IF (NXI0.NE.NXIPV) THEN
         CALL ULIBER (45,
     1' IDSFFT (BIVAR) - MD=3 BUT NXI WAS CHANGED SINCE LAST CALL',
     2                58)
         STOP 'ULIBER45'
      ENDIF
      IF (NYI0.NE.NYIPV) THEN
         CALL ULIBER (46,
     1' IDSFFT (BIVAR) - MD=3 BUT NYI WAS CHANGED SINCE LAST CALL',
     2                58)
         STOP 'ULIBER46'
      ENDIF
C ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3)
   30 JWIPT=16
      JWIWL=6*NDP0+1
      JWNGP0=JWIWL-1
      JWIPL=24*NDP0+1
      JWIWP=30*NDP0+1
      JWIGP0=31*NDP0
      JWWPD=5*NDP0+1
C TRIANGULATES THE X-Y PLANE.  (FOR MD=1)
   40 IF(MD0.GT.1)   GO TO 41
      CALL IDTANG(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            IWK(JWIWL),IWK(JWIWP),WK)
      IWK(5)=NT
      IWK(6)=NL
      IF(NT.EQ.0)    RETURN
      GO TO 50
   41 NT=IWK(5)
      NL=IWK(6)
C SORTS OUTPUT GRID POINTS IN ASCENDING ORDER OF THE TRIANGLE
C NUMBER AND THE BORDER LINE SEGMENT NUMBER.  (FOR MD=1,2)
   50 IF(MD0.GT.2)   GO TO 60
      CALL IDGRID(XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),NXI0,NYI0,
     1            XI,YI,IWK(JWNGP0+1),IWK(JWIGP0+1))
C ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS.
C (FOR MD=1,2,3)
   60 CALL IDPDRV(NDP0,XD,YD,ZD,NT,IWK(JWIPT),WK,WK(JWWPD))
C INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3)
   70 ITPV=0
      JIG0MX=0
      JIG1MN=NXI0*NYI0+1
      NNGP=NT+2*NL
      DO 79  JNGP=1,NNGP
        ITI=JNGP
        IF(JNGP.LE.NT)    GO TO 71
        IL1=(JNGP-NT+1)/2
        IL2=(JNGP-NT+2)/2
        IF(IL2.GT.NL)     IL2=1
        ITI=IL1*(NT+NL)+IL2
   71   JWNGP=JWNGP0+JNGP
        NGP0=IWK(JWNGP)
        IF(NGP0.EQ.0)     GO TO 76
        JIG0MN=JIG0MX+1
        JIG0MX=JIG0MX+NGP0
        DO 72  JIGP=JIG0MN,JIG0MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI0+1
          IXI=IZI-NXI0*(IYI-1)
          CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1               ITI,XI(IXI),YI(IYI),ZI(IXI,IYI))
   72   CONTINUE
   76   JWNGP=JWNGP0+2*NNGP+1-JNGP
        NGP1=IWK(JWNGP)
        IF(NGP1.EQ.0)     GO TO 79
        JIG1MX=JIG1MN-1
        JIG1MN=JIG1MN-NGP1
        DO 77  JIGP=JIG1MN,JIG1MX
          JWIGP=JWIGP0+JIGP
          IZI=IWK(JWIGP)
          IYI=(IZI-1)/NXI0+1
          IXI=IZI-NXI0*(IYI-1)
          CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1               ITI,XI(IXI),YI(IYI),ZI(IXI,IYI))
   77   CONTINUE
   79 CONTINUE
      RETURN
      END
