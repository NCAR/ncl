C
C     SUBROUTINE IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,IWK,WK)
C
C DIMENSION OF            XD(NDP), YD(NDP), ZD(NDP), XI(NIP), YI(NIP)
C ARGUMENTS               ZI(NIP), IWK(31*NDP+NIP),  WK(8*NDP)
C
C PURPOSE                 TO PERFORM BIVARIATE INTERPOLATION WHEN THE
C                         PROJECTIONS OF THE DATA POINTS IN THE X-Y
C                         PLANE ARE IRREGULARLY DISTRIBUTED.
C
C USAGE                   CALL IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,
C                                      IWK,WK)
C
C ARGUMENTS
C
C ON INPUT                MD
C                           MODE OF COMPUTATION (MUST BE 1, 2, OR 3,
C                           ELSE AN ERROR RETURN OCCURS.)
C                           = 1 IF THIS IS THE FIRST CALL TO THIS
C                               SUBROUTINE, OR IF THE VALUE OF NDP
C                               HAS BEEN CHANGED FROM THE PREVIOUS
C                               CALL, OR IF THE CONTENTS OF THE XD
C                               OR YD ARRAYS HAVE BEEN CHANGED FROM
C                               THE PREVIOUS CALL.
C                           = 2 IF THE VALUES OF NDP AND THE XD AND
C                               YD ARRAYS ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, BUT NEW VALUES FOR
C                               XI, YI ARE BEING USED.  IF MD = 2
C                               AND NDP HAS BEEN CHANGED SINCE THE
C                               PREVIOUS CALL TO IDBVIP, AN ERROR
C                               RETURN OCCURS.
C                           = 3 IF THE VALUES OF  NDP, NIP, XD,
C                               YD, XI, YI ARE UNCHANGED FROM THE
C                               PREVIOUS CALL, I.E. IF THE ONLY
C                               CHANGE ON INPUT TO IDBVIP IS IN THE
C                               ZD ARRAY.  IF MD=3 AND NDP OR NIP HAS
C                               BEEN CHANGED SINCE THE PREVIOUS CALL
C                               TO IDBVIP, AN ERROR RETURN OCCURS.
C
C                           BETWEEN THE CALL WITH MD=2 OR MD=3 AND
C                           THE PRECEDING CALL, THE IWK AND WK WORK
C                           ARRAYS SHOULD NOT BE DISTURBED.
C
C                        NDP
C                          NUMBER OF DATA POINTS (MUST BE 4 OR
C                          GREATER, ELSE AN ERROR RETURN OCCURS).
C
C                        XD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE
C                          X COORDINATES OF THE DATA POINTS.
C
C                        YD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE
C                          Y COORDINATES OF THE DATA POINTS.
C
C                        ZD
C                          ARRAY OF DIMENSION  NDP  CONTAINING THE
C                          Z COORDINATES OF THE DATA POINTS.
C
C                        NIP
C                          THE NUMBER OF OUTPUT POINTS AT WHICH
C                          INTERPOLATION IS TO BE PERFORMED (MUST BE
C                          1 OR GREATER, ELSE AN ERROR RETURN OCCURS).
C
C                        XI
C                          ARRAY OF DIMENSION  NIP  CONTAINING THE X
C                          COORDINATES OF THE OUTPUT POINTS.
C
C                        YI
C                          ARRAY OF DIMENSION  NIP  CONTAINING THE Y
C                          COORDINATES OF THE OUTPUT POINTS.
C
C                        IWK
C                          INTEGER WORK ARRAY OF DIMENSION AT LEAST
C                          31*NDP + NIP
C
C                        WK
C                          REAL WORK ARRAY OF DIMENSION AT LEAST 8*NDP
C
C ON OUTPUT               ZI
C                           ARRAY OF DIMENSION NIP WHERE INTERPOLATED
C                           Z  VALUES ARE TO BE STORED.
C
C SPECIAL CONDITIONS     INADEQUATE WORK SPACE IWK AND WK MAY
C                        MAY CAUSE INCORRECT RESULTS.
C
C                        THE DATA POINTS MUST BE DISTINCT AND THEIR
C                        PROJECTIONS IN THE X-Y PLANE MUST NOT BE
C                        COLLINEAR, OTHERWISE AN ERROR RETURN OCCURS.
C ********************************************************************
      SUBROUTINE  IDBVIP(MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,
     1                   IWK,WK)
C THIS SUBROUTINE CALLS THE IDLCTN, IDPDRV, IDPTIP, AND IDTANG
C SUBROUTINES.
C DECLARATION STATEMENTS
      DIMENSION XD(NDP), YD(NDP), ZD(NDP),           XI(NIP),
     1          YI(NIP), ZI(NIP), IWK(31*NDP + NIP), WK(8*NDP)
      COMMON/IDLC/ITIPV,DMMY1(13)
      COMMON/IDPT/ITPV,DMMY(27)
C
C  THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
C SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES.
C (FOR MD=1,2,3)
   10 MD0=MD
      NDP0=NDP
      NIP0=NIP
C ERROR CHECK.  (FOR MD=1,2,3)
   20 IF (MD0.LT.1.OR.MD0.GT.3) THEN
         CALL ULIBER (32,
     1' IDBVIP (BIVAR) - INPUT PARAMETER MD OUT OF RANGE',49)
         STOP 'ULIBER32'
      ENDIF
      IF (NDP0.LT.4) THEN
         CALL ULIBER (33,
     1' IDBVIP (BIVAR) - INPUT PARAMETER NDP OUT OF RANGE',50)
         STOP 'ULIBER33'
      ENDIF
      IF (NIP0.LT.1) THEN
         CALL ULIBER (34,
     1' IDBVIP (BIVAR) - INPUT PARAMETER NIP OUT OF RANGE',50)
         STOP 'ULIBER34'
      ENDIF
      IF(MD0.GT.1)        GO TO 21
      IWK(1)=NDP0
      GO TO 22
   21 NDPPV=IWK(1)
      IF (NDP0.NE.NDPPV) THEN
         CALL ULIBER (50,
     1' IDBVIP (BIVAR) - MD=2 OR 3 BUT NDP WAS CHANGED SINCE LAST CALL',
     2                63)
         STOP 'ULIBER50'
      ENDIF
   22 IF(MD0.GT.2)        GO TO 23
      IWK(3)=NIP
      GO TO 30
   23 NIPPV=IWK(3)
      IF (NIP0.LT.NIPPV) THEN
         CALL ULIBER (51,
     1' IDBVIP (BIVAR) - MD=3 BUT NIP WAS CHANGED SINCE LAST CALL',
     2                58)
         STOP 'ULIBER51'
      ENDIF
C ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3)
   30 JWIPT=16
      JWIWL=6*NDP0+1
      JWIWK=JWIWL
      JWIPL=24*NDP0+1
      JWIWP=30*NDP0+1
      JWIT0=31*NDP0
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
C LOCATES ALL POINTS AT WHICH INTERPOLATION IS TO BE PERFORMED.
C (FOR MD=1,2)
   50 IF(MD0.GT.2)   GO TO 60
      ITIPV=0
      JWIT=JWIT0
      DO 51  IIP=1,NIP0
        JWIT=JWIT+1
        CALL IDLCTN(NDP0,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),
     1            XI(IIP),YI(IIP),IWK(JWIT),IWK(JWIWK),WK)
   51 CONTINUE
C ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS.
C (FOR MD=1,2,3)
   60 CALL IDPDRV(NDP0,XD,YD,ZD,NT,IWK(JWIPT),WK,WK(JWWPD))
C INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3)
   70 ITPV=0
      JWIT=JWIT0
      DO 71  IIP=1,NIP0
        JWIT=JWIT+1
        CALL IDPTIP(XD,YD,ZD,NT,IWK(JWIPT),NL,IWK(JWIPL),WK,
     1              IWK(JWIT),XI(IIP),YI(IIP),ZI(IIP))
   71 CONTINUE
      RETURN
      END
