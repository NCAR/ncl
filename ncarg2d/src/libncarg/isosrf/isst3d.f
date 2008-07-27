C
C $Id: isst3d.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISST3D (EYE,ULO,UHI,VLO,VHI,WLO,WHI)
C
      DIMENSION       EYE(3)
C
C This routine determines how the picture will be scaled onto the
C plotter frame.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
      COMMON /PWRZ1I/ UUMIN,UUMAX,VVMIN,VVMAX,WWMIN,WWMAX,DELCRT,EYEU,
     +                EYEV,EYEW
      SAVE   /PWRZ1I/
C
      COMMON /TEMPRX/ RZERO
      SAVE   /TEMPRX/
C
C Arithmetic statement function for taking the average of two reals.
C
      AVE(A,B) = (A+B)*.5
C
C Arithmetic statement functions for scaling.
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
C Constants for PWRZI.
C
      UUMIN = ULO
      UUMAX = UHI
      VVMIN = VLO
      VVMAX = VHI
      WWMIN = WLO
      WWMAX = WHI
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
C
C Find corners in 2-space for 3-space box containing object.
C
      ISCALE = 0
      ATU = AVE(SU(UUMIN),SU(UUMAX))
      ATV = AVE(SV(VVMIN),SV(VVMAX))
      ATW = AVE(SW(WWMIN),SW(WWMAX))
      BIGD = 0.
      IF (RZERO .LE. 0.) GO TO  10
C
C Relative size feature is in use.  Generate eye position that makes box
C have maximum projected size.
C
      ALPHA = -(VVMIN-ATV)/(UUMIN-ATU)
      VVEYE = -RZERO/SQRT(1.+ALPHA*ALPHA)
      UUEYE = VVEYE*ALPHA
      VVEYE = VVEYE+ATV
      UUEYE = UUEYE+ATU
      WWEYE = ATW
      CALL ISTR32 (ATU,ATV,ATW,UUEYE,VVEYE,WWEYE,1)
      CALL ISTR32 (UUMIN,VVMIN,ATW,XMIN,DUMM,DUMM,2)
      CALL ISTR32 (UUMAX,VVMIN,WWMIN,DUMM,YMIN,DUMM,2)
      CALL ISTR32 (UUMAX,VVMAX,ATW,XMAX,DUMM,DUMM,2)
      CALL ISTR32 (UUMAX,VVMIN,WWMAX,DUMM,YMAX,DUMM,2)
      BIGD = SQRT((UUMAX-UUMIN)**2+(VVMAX-VVMIN)**2+(WWMAX-WWMIN)**2)*.5
      R0 = RZERO
      GO TO  20
   10 CALL ISTR32 (ATU,ATV,ATW,EYE(1),EYE(2),EYE(3),1)
      CALL ISTR32 (SU(UUMIN),SV(VVMIN),SW(WWMIN),X1,Y1,DUM,2)
      CALL ISTR32 (SU(UUMIN),SV(VVMIN),SW(WWMAX),X2,Y2,DUM,2)
      CALL ISTR32 (SU(UUMIN),SV(VVMAX),SW(WWMIN),X3,Y3,DUM,2)
      CALL ISTR32 (SU(UUMIN),SV(VVMAX),SW(WWMAX),X4,Y4,DUM,2)
      CALL ISTR32 (SU(UUMAX),SV(VVMIN),SW(WWMIN),X5,Y5,DUM,2)
      CALL ISTR32 (SU(UUMAX),SV(VVMIN),SW(WWMAX),X6,Y6,DUM,2)
      CALL ISTR32 (SU(UUMAX),SV(VVMAX),SW(WWMIN),X7,Y7,DUM,2)
      CALL ISTR32 (SU(UUMAX),SV(VVMAX),SW(WWMAX),X8,Y8,DUM,2)
      XMIN = MIN(X1,X2,X3,X4,X5,X6,X7,X8)
      XMAX = MAX(X1,X2,X3,X4,X5,X6,X7,X8)
      YMIN = MIN(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
      YMAX = MAX(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
C
C Add the right amount to keep the picture square.
C
   20 WIDTH = XMAX-XMIN
      HIGHT = YMAX-YMIN
      DIF = .5*(WIDTH-HIGHT)
      IF (DIF)  30, 50, 40
   30 XMIN = XMIN+DIF
      XMAX = XMAX-DIF
      GO TO  50
   40 YMIN = YMIN-DIF
      YMAX = YMAX+DIF
   50 ISCALE = 1
      CALL ISTR32 (ATU,ATV,ATW,EYE(1),EYE(2),EYE(3),1)
      RETURN
      END
