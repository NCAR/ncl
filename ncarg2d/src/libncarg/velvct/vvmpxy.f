C
C       $Id: vvmpxy.f,v 1.13 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVMPXY (X,Y,U,V,UVM,XB,YB,XE,YE,IST)
C
C This routine incorporates the functions previously known as FX, FX, 
C MXF, and MYF into a single subroutine. However, a compatibility
C mode is retained so that old codes that depend on custom versions of
C these functions will still work. The recommended approach for new 
C code that requires a custom transformation routine is to write your
C own version of VVUMXY (Velocity Vector -- User Map X,Y). Whenever 
C IMAP has a value other than 0,1, or 2 VVUMXY will be called.
C
C The most important thing to note is that the input vector position
C (a single point -- input parameters X and Y) is in user coordinate
C space, while the output (two points defining the beginning and
C end of the vector, XB,YB and XE,YE) is in the fractional coordinate
C space. 
C
C Note that each vector consists of three potentially tranformable
C components: position, direction, and magnitude. Depending on the 
C user's requirement, any or all of these components may need to be
C transformed. As an example the map transformations provided by
C VVMPXY transform the position and direction of the vector, but keep
C the vector magnitude based on a uniform coordinate system. This
C is the motivation for having the input vector position in user
C coordinates, but the output in the fractional coordinate system.
C The fractional system is known to be uniform -- a unit in X always
C equals a unit in Y.    
C
C Input parameters:
C
C X,Y   -- vector position in the user coordinate system
C U,V   -- vector components from the U,V arrays for this position
C UVM   -- vector magnitude,  (SQRT(U*U+V*V))
C
C Output parameters:
C
C XB,YB -- starting point of the vector in the fractional 
C          coordinate system
C XE,YE -- ending point of the vector in the fractional
C          coordinate system
C IST   -- status results of the mapping: 0 indicates success
C 
C
C The mapping common block: made available to user mapping routines
C
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
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
C --------------------------------------------------------------------
C
C Local parameters:
C
C PRCFAC - Precision factor used to resolve float equality within
C            the precision of a 4 byte REAL
C PVFRAC - Initial fraction of the vector magnitude used to
C            determine the differential increment
C PFOVFL - Floating point overflow value
C IPMXCT - Number of times to allow the differential to increase
C PDUVML - Multiplier when the differential is increased
C PCSTST - Test value for closeness to 90 degree latitude
C
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.001,
     +           PFOVFL=1E12,
     +           IPMXCT=40,
     +           PDUVML=2.0,
     +           IPCTST=PRCFAC*90)
C
C Local variables:
C
C DUV   - incremental UV magnitude difference
C CLT   - cosine of the latitude
C ICT   - repetition count for the difference loop
C SGN   - sign factor, for switching from pos to neg difference
C XT,YT - temporary projected X,Y position values
C XTF,YTF - temporary X,Y fractional coordinate values
C DV1,DV2 - vector magnitudes
C T     - temporary value
C IZO   - zero magnitude flag
C
C Each end of the vector is calculated in the user coordinate space.
C
C Zero the status flag
C
      IST=0
C
C Check for zero magnitude
C
      IF (INT(UVM*PRCFAC+0.5) .EQ. 0) THEN
         IZO = 1
      ELSE
         IZO = 0
      END IF
C
      If (IMAP .EQ. 0) THEN
C
C Linear transformation
C
         IF (X .LT. WXMN .OR. X .GT. WXMX .OR.
     +        Y .LT. WYMN .OR. Y .GT. WYMX) THEN
            IST=-5
            RETURN
         ENDIF
         XB=CUFX(X)
         YB=CUFY(Y)
C
C Return special status if zero magnitude
C
         IF (IZO .EQ. 1) THEN
            XE = XB
            YE = YB
            IST = -999
            RETURN
         END IF
C
         XE=XB+U*SXDC
         YE=YB+V*SYDC
C
C The following code accounts for non-uniform window coordinate
C systems, such as log scaled coordinates. The technique used
C is similar to the one used in the EZMAP case
C
         IF (ITRT .EQ. 1) THEN
C     
            DV1=SQRT((XE-XB)*(XE-XB)+(YE-YB)*(YE-YB))
C
C Set up an initial increment factor
C
            DUV=0.1/UVM
            ICT=0
            SGN=1.0
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
            XT=X+SGN*U*DUV
            YT=Y+SGN*V*DUV
            IF (XT .LT. WXMN .OR. XT .GT. WXMX .OR.
     +           YT .LT. WYMN .OR. YT .GT. WYMX) THEN
               IF (SGN .EQ. 1.0) THEN
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
            DV2=SQRT((XTF-XB)*(XTF-XB)+(YTF-YB)*(YTF-YB))
            IF (DV2*1E3 .GT. DVMX) THEN
               ICT=ICT+1
               DUV=DUV/2.0
               GO TO 10
            ELSE IF (DV2*1E4 .LT. DVMX) THEN
               ICT=ICT+1
               DUV=DUV*2.0
               GO TO 10
            END IF
C
C The actual endpoints are found using the ratio of the incremental
C distance to the actual distance times the fractional component
C length
            XE=XB+SGN*(XTF-XB)*DV1/DV2
            YE=YB+SGN*(YTF-YB)*DV1/DV2
C
         END IF
C
C EZMAP overlay
C
      ELSE IF (IMAP .EQ. 1) THEN
C
C X is longitude, Y is latitude
C If Y is 90 degrees, a directional vector is meaningless
C Since EZMAP uses uniform user coordinates, don't convert to 
C fractional coordinates until the end
C The tranformation type parameter is not used.
C
         IF (INT(ABS(Y)*PRCFAC+0.5).EQ.IPCTST) THEN
            IST=-1
            RETURN
         END IF
C
C Project the starting value: bail out if outside the window
C
         CALL MAPTRA (Y,X,XB,YB)
         IF (XB .LT. WXMN .OR. XB .GT. WXMX .OR.
     +       YB .LT. WYMN .OR. YB .GT. WYMX) THEN
            IST=-5
            RETURN
         END IF
C
C Return special status if zero magnitude
C
         IF (IZO .EQ. 1) THEN
            XE = XB
            YE = YB
            IST = -999
            RETURN
         END IF
C
C The incremental distance is proportional to a small fraction
C of the vector magnitude
C
         DUV=PVFRAC/UVM
         CLT=COS(Y*PDTOR)
C
C Project the incremental distance. If the positive difference doesn't
C work, try the negative difference. If the difference results in a 
C zero length vector, try a number of progressively larger increments. 
C
         ICT=0
         SGN=1.0
 20      CONTINUE

         CALL MAPTRA(Y+SGN*V*DUV,X+SGN*U*DUV/CLT,XT,YT)

         DV1=SQRT((XT-XB)*(XT-XB)+(YT-YB)*(YT-YB))
         IF (DV1 .GT. RLEN) THEN
            IF (SGN .EQ. -1.0) THEN
               IST=-4
               RETURN
            ELSE
               SGN=-1.0
               GO TO 20
            END IF
         END IF
C
         IF (INT(DV1*PRCFAC) .EQ. 0) THEN
            IF (ICT .LT. IPMXCT) THEN
               ICT = ICT + 1
               DUV=DUV*PDUVML
               GO TO 20
            ELSE
               IST=-3
               RETURN
            END IF
         END IF
C
         IF (ABS(XT) .GE. PFOVFL .OR. ABS(YT) .GE. PFOVFL) THEN
            IST=-6
            RETURN
         END IF
C
         T=SGN*((XT-XB)/DV1)*UVM
         XB=CUFX(XB)
         XE=XB+T*SXDC
         T=SGN*((YT-YB)/DV1)*UVM
         YB=CUFY(YB)
         YE=YB+T*SYDC
C
      ELSE IF (IMAP .EQ. 2) THEN
C
C ... the polar coordinate case ...
C
         TH=PDTOR*Y
         XB=CUFX(X*COS(TH))
         YB=CUFY(X*SIN(TH))
C
C Return special status if zero magnitude
C
         IF (IZO .EQ. 1) THEN
            XE = XB
            YE = YB
            IST = -999
            RETURN
         END IF
C
         XE=XB+U*SXDC
         YE=YB+V*SYDC
C
         IF (ITRT.GE.1) THEN
            DV1=SQRT((XE-XB)*(XE-XB)+(YE-YB)*(YE-YB))
            PHI=ATAN2(V,U)
            ANG=TH+PHI
            XE=XB + DV1*COS(ANG)
            YE=YB + DV1*SIN(ANG)
         END IF
C
C ... and everything else.
C
      ELSE
C
         CALL VVUMXY(X,Y,U,V,UVM,XB,YB,XE,YE,IST)
C
      END IF
C
C Done.
C
      RETURN
C
      END




