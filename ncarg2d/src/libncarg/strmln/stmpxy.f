C
C       $Id: stmpxy.f,v 1.13 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STMPXY(XDA,YDA,XUS,YUS,IST)
C
C Transform a point in data coordinate space to user space
C
C Input parameters:
C
C XDA,YDA - Point in data coordinate space
C
C Output parameters:
C
C XUS,YUS - Point in user coordinate space
C IST     - Status code indicating success or failure
C
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C -------------------------------------------------------------
C
      IST=0
      IF (IMAP .EQ. 0) THEN
         XUS=XDA
         YUS=YDA
      ELSE IF (IMAP .EQ. 1) THEN
         CALL MAPTRA(YDA,XDA,XUS,YUS)
      ELSE IF (IMAP .EQ. 2) THEN
         XUS=XDA*COS(PDTOR*YDA)
         YUS=XDA*SIN(PDTOR*YDA)
      ELSE
         CALL STUMXY(XDA,YDA,XUS,YUS,IST)
      END IF
C
      IF (XUS.LT.WXMN .OR. XUS.GT.WXMX
     +     .OR. YUS.LT.WYMN .OR. YUS.GT.WYMX) THEN
         IST=-1
      END IF
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STIMXY(XUS,YUS,XDA,YDA,IST)
C
C Inversely transform a point in user coordinate space to data space
C
C Input parameters:
C
C XUS,YUS - Point in user coordinate space
C
C Output parameters:
C
C XDA,YDA - Point in data coordinate space
C IST     - Status code indicating success or failure
C
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C ---------------------------------------------------------------------
C
      IST=0
      IF (IMAP .EQ. 0) THEN
         XDA=XUS
         YDA=YUS
      ELSE IF (IMAP .EQ. 1) THEN
         CALL MAPTRI(XUS,YUS,YDA,XDA)
         IF (XDA .EQ. 1.E12 .OR. YDA .EQ. 1.E12) THEN
            IST = -1
            RETURN
         ENDIF
C
C MAPTRI always returns longitudes in the ranges -180. to +180.
C However, the user is allowed to express the data boundaries
C anywhere within the range -540. to +540. Therefore, the
C longitudinal values returned by MAPTRI may need adjustment.
C Latitudes are not adjusted, however.
C
         TLO = MIN(XLOV,XHIV)
         THI = MAX(XLOV,XHIV)
         IF (XDA .LT. TLO) THEN
            XDA = XDA + 360.0
            IF (XDA .GT. THI .OR. XDA .LT. TLO) THEN
               IST = -1
               RETURN
            END IF
         ELSE IF (XDA .GT. THI) THEN
            XDA = XDA - 360.0
            IF (XDA .LT. TLO .OR. XDA .GT. THI) THEN
               IST = -1
               RETURN
            ENDIF
         ENDIF
C
         IF (YDA.LT.MIN(YLOV,YHIV) .OR. YDA.GT.MAX(YLOV,YHIV)) THEN
            IST = -1
            RETURN
         ENDIF
C
       ELSE IF (IMAP .EQ. 2) THEN
         XDA=SQRT(XUS*XUS+YUS*YUS)
         YDA=PRTOD*ATAN2(YUS,XUS)
C
C Polar mapping has special bounds checking requirements.
C If YDA is less than YLOV, it is possible the user intends
C to treat some or all of quandrant 3 and 4 as positive angles
C (unlike ATAN2). Add 360 in this case.
C
         IF (YDA.LT.YLOV) YDA=YDA+360.0
C
      ELSE
         CALL STUIXY(XUS,YUS,XDA,YDA,IST)
      END IF
C
C Check for out of bound conditions
C
      IF (XDA.LT.MIN(XLOV,XHIV) .OR. XDA.GT.MAX(XHIV,XLOV)
     +     .OR. YDA.LT.MIN(YLOV,YHIV) .OR. YDA.GT.MAX(YHIV,YLOV)) THEN
         IST=-1
      END IF
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
C Map tangent angle to normalized device coordinate space.
C
C Input parameters:
C
C XDA,YDA - Point in data coordinate space
C XUS,YUS - Point in user coordinate space
C XND,YND - Point in NDC space
C DU,DV   - Differential vector components in data space
C
C Output parameters:
C
C TA      - Streamline tangent angle in NDC space
C IST     - Status code indicating success or failure
C
C In general the distance over which the angle is measured is 
C iteratively adjusted until it is becomes a small (but not too small)
C fraction of the fixed (in NDC space) increment distance.
C The transformation type parameter is implemented for the polar
C mapping only.
C
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C ---------------------------------------------------------------------
C
C Local parameters:
C
C PRCFAC - Precision factor used to resolve float equality within
C            the precision of a 4 byte REAL
C PVFRAC - Initial fraction of the vector magnitude used to
C            determine the differential increment
C IPMXCT - Number of times to allow the differential to increase
C PDUVML - Multiplier when the differential is increased
C PCSTST - Test value for closeness to 90 degree latitude
C
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.1,
     +           IPMXCT=40,
     +           PDUVML=2.0,
     +           PCSTST=PRCFAC*90.0)
C
C Local variables:
C
C XE,YE   - Current end of the temporary vector
C DV1     - First vector magnitude
C SGN     - Sign factor applied to differential
C DUV     - Incremental magnitude multiplier
C ICT     - Count of iterations
C XT,YT   - Temporary X,Y coordinates (User space)
C XTF,YTF - Temporary X,Y coordinates (NDC space)
C DV2     - Second vector magnitude
C CLT     - Cosine of the latitude
C DTX,DTY - Diffential values after accounting for the latitude
C DNX,DNY - Trial incremental values
C TH      - Angle theta
C
C ---------------------------------------------------------------------
C
      IST=0
C
      If (IMAP .EQ. 0) THEN
C
         XE=XND+DU
         YE=YND+DV
C
C If space mapping then more code is required
C
         IF (ITRT.GE.1) THEN
C
            DV1=SQRT((XE-XND)*(XE-XND)+(YE-YND)*(YE-YND))
C
C Set up an initial increment factor
C
            DUV=PVFRAC/VNML
            SGN=1.0
            ICT=0
C     
 10         CONTINUE
C
C Bail out if it's not working
C     
            IF (ICT .GT. IPMXCT) THEN
               IST = -3
               RETURN
            END IF
C
C Calculate the incremental end points, then check to see if 
C they take us out of the user coordinate boundaries. If they
C do, try incrementing in the other direction
C
            XT=XUS+SGN*DU*DUV
            YT=YUS+SGN*DV*DUV
            IF (XT .LT. WXMN .OR. XT .GT. WXMX .OR.
     +           YT .LT. WYMN .OR. YT .GT. WYMX) THEN
               IF (SGN.EQ.1.0) THEN
                  SGN = -1.0
                  GO TO 10
               ELSE
                  IST=-4
                  RETURN
               ENDIF
            END IF
C
C Convert to fractional coordinates and find the incremental
C distance in the fractional system. To ensure that this distance
C is meaningful, we require that it be between 1E3 and 1E4
C times smaller than the maximum vector length.
C 
            XTF=CUFX(XT)
            YTF=CUFY(YT)
            DV2=SQRT((XTF-XND)*(XTF-XND)+(YTF-YND)*(YTF-YND))
            IF (DV2*1E1 .GT. DFMG) THEN
               ICT=ICT+1
               DUV=DUV/PDUVML
               GO TO 10
            ELSE IF (DV2*1E2 .LT. DFMG) THEN
               ICT=ICT+1
               DUV=DUV*PDUVML
               GO TO 10
            END IF
C
C The actual endpoints are found using the ratio of the incremental
C distance to the actual distance times the fractional component
C length
            XE=XND+SGN*(XTF-XND)*DV1/DV2
            YE=YND+SGN*(YTF-YND)*DV1/DV2
C
         END IF
C
C Calculate the angle
C
         TA=ATAN2((YE-YND),(XE-XND))
C
C EZMAP overlay
C
      ELSE IF (IMAP .EQ. 1) THEN
C
C X is longitude, Y is latitude
C If Y is 90 degrees, can't compute a direction
C
         IF (INT(ABS(YDA)*PRCFAC+0.5) .GE. INT(PCSTST)) THEN
            IST=-1
            RETURN
         END IF
C
         SGN=1.0
         DUV=PVFRAC/VNML
         CLT=COS(YDA*PDTOR)
         DTX=DU/CLT
         DTY=DV
         ICT=0
C
 20      CONTINUE
C
         DNX=DTX*DUV
         DNY=DTY*DUV
C
C Bail out if it's not working
C     
         IF (ICT .GT. IPMXCT) THEN
            IST = -3
            RETURN
         END IF
C
C Calculate the incremental end points, then check to see if 
C they take us out of the user coordinate boundaries. If they
C do, try incrementing in the other direction
C
         CALL MAPTRA(YDA+SGN*DNY,XDA+SGN*DNX,XT,YT)
C
         IF (XT .LT. WXMN .OR. XT .GT. WXMX .OR.
     +        YT .LT. WYMN .OR. YT .GT. WYMX) THEN
            IF (SGN .EQ. 1) THEN
               SGN=-1.0
               GO TO 20
            ELSE
               IST=-4
               RETURN
            ENDIF
         END IF
C
C Convert to fractional coordinates and find the incremental
C distance in the fractional system. To ensure that this distance
C is meaningful, we require that it be between 1E3 and 1E4
C times smaller than the maximum vector length.
C 
         XTF=CUFX(XT)
         YTF=CUFY(YT)
         DV2=SQRT((XTF-XND)*(XTF-XND)+(YTF-YND)*(YTF-YND))
         IF (DV2*1E1 .GT. DFMG) THEN
            ICT=ICT+1
            DUV=DUV/PDUVML
            GO TO 20
         ELSE IF (DV2*1E2 .LT. DFMG) THEN
            ICT=ICT+1
            DUV=DUV*PDUVML
            GO TO 20
         END IF
C
         TA=ATAN2((SGN*(YTF-YND)),(SGN*(XTF-XND)))
C
      ELSE IF (IMAP .EQ. 2) THEN
C
C ... the polar coordinate case ...
C
         TA=ATAN2(DV,DU)
C
         IF (ITRT.GE.1) THEN
            TH=PDTOR*YDA
            TA=TH+TA
         END IF
C
C ... and everything else.
C
      ELSE
C
         CALL STUMTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
      END IF
C
C Done.
C
      RETURN
C
      END
