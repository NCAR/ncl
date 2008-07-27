C
C $Id: trn32t.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TRN32T (U,V,W,XT,YT,ZT,IENT)
C
C THIS ROUTINE IMPLEMENTS THE 3-SPACE TO 2-SPACE TRANSFOR-
C MATION BY KUBER, SZABO AND GIULIERI, THE PERSPECTIVE
C REPRESENTATION OF FUNCTIONS OF TWO VARIABLES. J. ACM 15,
C 2, 193-204,1968.
C TRN32T ARGUMENTS
C U,V,W    ARE THE 3-SPACE COORDINATES OF THE INTERSECTION
C          OF THE LINE OF SIGHT AND THE IMAGE PLANE.  THIS
C          POINT CAN BE THOUGHT OF AS THE POINT LOOKED AT.
C XT,YT,ZT ARE THE 3-SPACE COORDINATES OF THE EYE POSITION.
C
C TRN32 ARGUMENTS
C U,V,W    ARE THE 3-SPACE COORDINATES OF A POINT TO BE
C          TRANSFORMED.
C XT,YT    THE RESULTS OF THE 3-SPACE TO 2-SPACE TRANSFOR-
C          MATION.  WHEN ISCALE=0, XT AND YT ANR IN THE SAME
C          UNITS AS U,V, AND W.  WHEN ISCALE'0, XT AND YT
C          ARE IN PLOTTER COORDINATES.
C ZT       NOT USED.
C
C
      SAVE
C
      COMMON /PWRZ1T/ UUMIN      ,UUMAX      ,VVMIN      ,VVMAX      ,
     1                WWMIN      ,WWMAX      ,DELCRT     ,EYEU       ,
     2                EYEV       ,EYEW
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX       ,YMIN       ,
     1                YMAX       ,BIGD       ,R0         ,NLX        ,
     2                NBY        ,NRX        ,NTY
C
C DECIDE IF SET OR TRANSLATE CALL
C
      IF (IENT .NE. 1) GO TO  50
C
C STORE THE PARAMETERS OF THE SET CALL
C FOR USE WITH THE TRANSLATION CALL
C
      AU = U
      AV = V
      AW = W
      EU = XT
      EV = YT
      EW = ZT
C
C
C
C
C
      DU = AU-EU
      DV = AV-EV
      DW = AW-EW
      D = SQRT(DU*DU+DV*DV+DW*DW)
      COSAL = DU/D
      COSBE = DV/D
      COSGA = DW/D
      AL = ACOS(COSAL)
      BE = ACOS(COSBE)
      GA = ACOS(COSGA)
      SINGA = SIN(GA)
C
C THE 3-SPACE POINT LOOKED AT IS TRANSFORMED INTO (0,0) OF
C THE 2-SPACE.  THE 3-SPACE W AXIS IS TRANSFORMED INTO THE
C 2-SPACE Y AXIS.  IF THE LINE OF SIGHT IS CLOSE TO PARALLEL
C TO THE 3-SPACE W AXIS, THE 3-SPACE V AXIS IS CHOSEN (IN-
C STEAD OF THE 3-SPACE W AXIS) TO BE TRANSFORMED INTO THE
C 2-SPACE Y AXIS.
C
      JDONE = 2
      IF (ISCALE)  10, 30, 10
   10 X0 = XMIN
      Y0 = YMIN
      X1 = NLX
      Y1 = NBY
      X2 = NRX-NLX
      Y2 = NTY-NBY
      X3 = X2/(XMAX-XMIN)
      Y3 = Y2/(YMAX-YMIN)
      X4 = NRX
      Y4 = NTY
      FACT = 1.
      IF (BIGD .LE. 0.) GO TO  20
      X0 = -BIGD
      Y0 = -BIGD
      X3 = X2/(2.*BIGD)
      Y3 = Y2/(2.*BIGD)
      FACT = R0/D
   20 DELCRT = X2
      JDONE = 1
   30 IF (SINGA .LT. 0.0001) GO TO  40
      R = 1./SINGA
      JUMP = 2
      RETURN
   40 SINBE = SIN(BE)
      R = 1./SINBE
      JUMP = 1
      RETURN
C
C********************  ENTRY TRN32  ************************
C     ENTRY TRN32 (U,V,W,XT,YT,ZT)
C
   50 UU = U
      VV = V
      WW = W
      Q = D/((UU-EU)*COSAL+(VV-EV)*COSBE+(WW-EW)*COSGA)
      GO TO (60, 70), JUMP
   60 UU = ((EW+Q*(WW-EW)-AW)*COSAL-(EU+Q*(UU-EU)-AU)*COSGA)*R
      VV = (EV+Q*(VV-EV)-AV)*R
      GO TO (80, 90), JDONE
   70 UU = ((EU+Q*(UU-EU)-AU)*COSBE-(EV+Q*(VV-EV)-AV)*COSAL)*R
      VV = (EW+Q*(WW-EW)-AW)*R
      GO TO (80, 90), JDONE
   80 XT = X1+X3*(FACT*UU-X0)
      YT = Y1+Y3*(FACT*VV-Y0)
      RETURN
   90 XT = UU
      YT = VV
      RETURN
      END
