C
C	$Id: stmpxy.f,v 1.1 1993-01-15 23:53:33 dbrown Exp $
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STMPXY(XWO,YWO,XUS,YUS,IST)
C
C Transform a point in world coordinate space to user space
C
C Input parameters:
C
C XWO,YWO - Point in world coordinate space
C
C Output parameters:
C
C XUS,YUS - Point in user coordinate space
C IST     - Status code indicating success or failure
C
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
C --------------------------------------------------------------------
C
C -------------------------------------------------------------
C
      IST=0
      IF (IMAP .EQ. 0) THEN
         XUS=XWO
         YUS=YWO
      ELSE IF (IMAP .EQ. 1) THEN
         CALL MAPTRA(YWO,XWO,XUS,YUS)
      ELSE IF (IMAP .EQ. 2) THEN
         XUS=XWO*COS(PDTOR*YWO)
         YUS=XWO*SIN(PDTOR*YWO)
      ELSE
         CALL STUMXY(XWO,YWO,XUS,YUS,IST)
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
      SUBROUTINE STIMXY(XUS,YUS,XWO,YWO,IST)
C
C Inversely transform a point in user coordinate space to world space
C
C Input parameters:
C
C XUS,YUS - Point in user coordinate space
C
C Output parameters:
C
C XWO,YWO - Point in world coordinate space
C IST     - Status code indicating success or failure
C
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
C --------------------------------------------------------------------
C
C ---------------------------------------------------------------------
C
      IST=0
      IF (IMAP .EQ. 0) THEN
         XWO=XUS
         YWO=YUS
      ELSE IF (IMAP .EQ. 1) THEN
         CALL MAPTRI(XUS,YUS,YWO,XWO)
      ELSE IF (IMAP .EQ. 2) THEN
         XWO=SQRT(XUS*XUS+YUS*YUS)
         YWO=PRTOD*ATAN2(YUS,XUS)
C
C Polar mapping has special bounds checking requirements.
C If YWO is less than YLOV, it is possible the user intends
C to treat some or all of quandrant 3 and 4 as positive angles
C (unlike ATAN2). Add 360 in this case.
C
         IF (YWO.LT.YLOV) YWO=YWO+360.0
C
      ELSE
         CALL STUIXY(XUS,YUS,XWO,YWO,IST)
      END IF
C
      IF (XWO.LT.XLOV .OR. XWO.GT.XHIV
     +     .OR. YWO.LT.YLOV .OR. YWO.GT.YHIV) THEN
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
      SUBROUTINE STMPTA(XWO,YWO,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
C Map tangent angle to normalized device coordinate space.
C
C Input parameters:
C
C XWO,YWO - Point in world coordinate space
C XUS,YUS - Point in user coordinate space
C XND,YND - Point in normalized device coordinate space
C DU,DV   - Differential vector components in world space
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
C --------------------------------------------------------------------
C
C ---------------------------------------------------------------------
C
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.1,
     +           PFOVFL=1E12,
     +           IPMXCT=40,
     +           PDUVML=2.0)
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
         DV1=SQRT((XE-XND)*(XE-XND)+(YE-YND)*(YE-YND))
C
C Set up an initial increment factor
C
         DUV=PVFRAC/VNML
         SGN=1.0
         ICT=0
C     
 10      CONTINUE
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
     +        YT .LT. WYMN .OR. YT .GT. WYMX) THEN
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
         TA=ATAN2((YE-YND),(XE-XND))
C
C EZMAP overlay
C
      ELSE IF (IMAP .EQ. 1) THEN
C
C X is longitude, Y is latitude
C If Y is 90 degrees, can't compute a direction
C
         IF (IFIX(ABS(YWO)*PRCFAC) .GE. IFIX(90.*PRCFAC)) THEN
            IST=-1
            RETURN
         END IF
C
         SGN=1.0
         DUV=PVFRAC/VNML
         CLT=COS(YWO*PDTOR)
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
         CALL MAPTRN(YWO+SGN*DNY,XWO+SGN*DNX,XT,YT)
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
         TA=ATAN2((SGN*YTF-YND),(SGN*XTF-XND))
C
      ELSE IF (IMAP .EQ. 2) THEN
C
C ... the polar coordinate case ...
C
         TA=ATAN2(DV,DU)
C
         IF (ITRT.GE.1) THEN
            TH=PDTOR*YWO
            TA=TH+TA
         END IF
C
C ... and everything else.
C
      ELSE
C
         CALL STUMTA(XWO,YWO,XUS,YUS,XND,YND,DU,DV,TA,IST)
C
      END IF
C
C Done.
C
      RETURN
C
      END
