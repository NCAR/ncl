C
C $Id: isosrf.f,v 1.8 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISOSRF (T,LU,MU,LV,MV,MW,EYE,MUVWP2,SLAB,TISO,IFLAG)
C
      DIMENSION T(LU,LV,MW),EYE(3),SLAB(MUVWP2,MUVWP2)
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Arithmetic statement function for taking an average of two reals.
C
      AVE(A,B) = (A+B)*.5
C
C Arithmetic statement functions for scaling.
C
      SU(UTEMP) = UTEMP
      SV(VTEMP) = VTEMP
      SW(WTEMP) = WTEMP
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL ISBLDA
C
C  Initialize machine constants.
C
      IF (IDONE.EQ.0) THEN
        CALL ISINIT
        IDONE=1
      END IF
C
C In 3-space, we use variables U, V, W, IU, IV, IW, etc.  In 2-space,
C we use X, Y, IX, IY, etc.  Initialize.
C
      NU = MU
      NUP2 = NU+2
      NV = MV
      NVP2 = NV+2
      NW = MW
      NWP2 = NW+2
      FNU = NU
      FNV = NV
      FNW = NW
      SU1 = SU(1.)
      SV1 = SV(1.)
      SW1 = SW(1.)
      SUNU = SU(FNU)
      SVNV = SV(FNV)
      SWNW = SW(FNW)
      AVEU = AVE(SU1,SUNU)
      AVEV = AVE(SV1,SVNV)
      AVEW = AVE(SW1,SWNW)
      EYEU = EYE(1)
      EYEV = EYE(2)
      EYEW = EYE(3)
      NUVWP2 = MUVWP2
      TVAL = TISO
      NFLAG = ABS(IFLAG)
      IF (NFLAG.EQ.0 .OR. NFLAG.GE.8) NFLAG = 7
C
C Set up scaling.
C
      CALL ISST3D (EYE,1.,FNU,1.,FNV,1.,FNW)
C
C Bound lower and left edge of slab.
C
      EDGE = -SIGN(BIG,REAL(IFLAG))
C
      DO  40 IUVW=1,NUVWP2
        SLAB(IUVW,1) = EDGE
        SLAB(1,IUVW) = EDGE
   40 CONTINUE
C
C Slices perpendicular to the U axis (V/W slices):
C
      IF (NFLAG .LT. 4) GO TO 100
      CALL ISZERO
C
      ISLBT = -1
C
C Determine whether the viewing direction turns the contours over.
C
      IF (EYEU.LT.AVEU) THEN
        IFLIP=1
      ELSE
        IFLIP=2
      END IF
C
      IF (IFLAG.LT.0) IFLIP=3-IFLIP
C
C Bound upper and right edge of slab.
C
      DO  50 IV=2,NVP2
        SLAB(IV,NWP2) = EDGE
   50 CONTINUE
      DO  60 IW=2,NWP2
        SLAB(NVP2,IW) = EDGE
   60 CONTINUE
C
C Go through 3D array in the U direction.  IUEW stands for "IU, Either
C Way".  Pick IU based on the relative values of AVEU and EYEU.
C
      DO  90 IUEW=1,NU
        IU = IUEW
        IF (EYEU .GT. AVEU) IU = NU+1-IUEW
        U = IU
C
C Load this slice of T into SLAB.
C
        IF (SVAL.EQ.0.) THEN
          DO  71 IV=1,NV
            DO  70 IW=1,NW
              SLAB(IV+1,IW+1) = T(IU,IV,IW)
   70       CONTINUE
   71     CONTINUE
        ELSE
          DO  73 IV=1,NV
            DO  72 IW=1,NW
              IF (T(IU,IV,IW).NE.SVAL) THEN
                SLAB(IV+1,IW+1) = T(IU,IV,IW)
              ELSE
                SLAB(IV+1,IW+1) = EDGE
              END IF
   72       CONTINUE
   73     CONTINUE
        END IF
C
C Contour this slab.
C
        CALL ISTRCL (SLAB,NUVWP2,NVP2,NWP2,TVAL)
C
C Construct the visibility array.
C
        CALL ISFILL
C
C If interpolation is turned on, fill in intermediate slabs.
C
        IF (NINU.NE.0.AND.IUEW.NE.NU) THEN
          IF (NINU.LT.0) ISDR=1
          IQ=IU+1
          IF (EYEU.GT.AVEU) IQ=IU-1
          DO  85 IINU=1,ABS(NINU)
            Q=REAL(IINU)/REAL(ABS(NINU)+1)
            P=1.-Q
            U=P*REAL(IU)+Q*REAL(IQ)
            IF (SVAL.EQ.0.) THEN
              DO  82 IV=1,NV
                DO  81 IW=1,NW
                  SLAB(IV+1,IW+1)=P*T(IU,IV,IW)+Q*T(IQ,IV,IW)
   81           CONTINUE
   82         CONTINUE
            ELSE
              DO  84 IV=1,NV
                DO  83 IW=1,NW
                  IF (T(IU,IV,IW).NE.SVAL.AND.T(IQ,IV,IW).NE.SVAL) THEN
                    SLAB(IV+1,IW+1)=P*T(IU,IV,IW)+Q*T(IQ,IV,IW)
                  ELSE
                    SLAB(IV+1,IW+1)=EDGE
                  END IF
   83           CONTINUE
   84         CONTINUE
            END IF
            CALL ISTRCL (SLAB,NUVWP2,NVP2,NWP2,TVAL)
            CALL ISFILL
   85     CONTINUE
          ISDR=0
        END IF
C
   90 CONTINUE
C
C Slices perpendicular to the V axis (U/W slices):
C
  100 IF (MOD(NFLAG/2,2) .EQ. 0) GO TO 160
      CALL ISZERO
C
      ISLBT = 0
C
C Determine whether the viewing direction turns the contours over.
C
      IF (EYEV.GT.AVEV) THEN
        IFLIP=1
      ELSE
        IFLIP=2
      END IF
C
      IF (IFLAG.LT.0) IFLIP=3-IFLIP
C
C Bound upper and right edge of slab.
C
      DO 110 IU=2,NUP2
        SLAB(IU,NWP2) = EDGE
  110 CONTINUE
      DO 120 IW=2,NWP2
        SLAB(NUP2,IW) = EDGE
  120 CONTINUE
C
C Go through 3D array in the V direction.  IVEW stands for "IV, Either
C Way".  Pick IV based on the relative values of AVEV and EYEV.
C
      DO 150 IVEW=1,NV
        IV = IVEW
        IF (EYEV .GT. AVEV) IV = NV+1-IVEW
        V = IV
C
C Load this slice of T into SLAB.
C
        IF (SVAL.EQ.0.) THEN
          DO 131 IU=1,NU
            DO 130 IW=1,NW
              SLAB(IU+1,IW+1) = T(IU,IV,IW)
  130       CONTINUE
  131     CONTINUE
        ELSE
          DO 133 IU=1,NU
            DO 132 IW=1,NW
              IF (T(IU,IV,IW).NE.SVAL) THEN
                SLAB(IU+1,IW+1) = T(IU,IV,IW)
              ELSE
                SLAB(IU+1,IW+1) = EDGE
              END IF
  132       CONTINUE
  133     CONTINUE
        END IF
C
C Contour this slab.
C
        CALL ISTRCL (SLAB,NUVWP2,NUP2,NWP2,TVAL)
C
C Construct the visibility array.
C
        CALL ISFILL
C
C If interpolation is turned on, fill in intermediate slabs.
C
        IF (NINV.NE.0.AND.IVEW.NE.NV) THEN
          IF (NINV.LT.0) ISDR=1
          IQ=IV+1
          IF (EYEV.GT.AVEV) IQ=IV-1
          DO 145 IINV=1,ABS(NINV)
            Q=REAL(IINV)/REAL(ABS(NINV)+1)
            P=1.-Q
            V=P*REAL(IV)+Q*REAL(IQ)
            IF (SVAL.EQ.0.) THEN
              DO 142 IU=1,NU
                DO 141 IW=1,NW
                  SLAB(IU+1,IW+1)=P*T(IU,IV,IW)+Q*T(IU,IQ,IW)
  141           CONTINUE
  142         CONTINUE
            ELSE
              DO 144 IU=1,NU
                DO 143 IW=1,NW
                  IF (T(IU,IV,IW).NE.SVAL.AND.T(IU,IQ,IW).NE.SVAL) THEN
                    SLAB(IU+1,IW+1)=P*T(IU,IV,IW)+Q*T(IU,IQ,IW)
                  ELSE
                    SLAB(IU+1,IW+1)=EDGE
                  END IF
  143           CONTINUE
  144         CONTINUE
            END IF
            CALL ISTRCL (SLAB,NUVWP2,NUP2,NWP2,TVAL)
            CALL ISFILL
  145     CONTINUE
          ISDR=0
        END IF
C
  150 CONTINUE
C
C Slices perpendicular to the W axis (U/V slices):
C
  160 IF (MOD(NFLAG,2) .EQ. 0) GO TO 220
      CALL ISZERO
C
      ISLBT = 1
C
C Determine whether the viewing direction turns the contours over.
C
      IF (EYEW.LT.AVEW) THEN
        IFLIP=1
      ELSE
        IFLIP=2
      END IF
C
      IF (IFLAG.LT.0) IFLIP=3-IFLIP
C
C Bound upper and right edge of slab.
C
      DO 170 IU=2,NUP2
        SLAB(IU,NVP2) = EDGE
  170 CONTINUE
      DO 180 IV=2,NVP2
        SLAB(NUP2,IV) = EDGE
  180 CONTINUE
C
C Go through 3D array in the W direction.  IWEW stands for "IW, Either
C Way".  Pick IW based on the relative values of AVEW and EYEW.
C
      DO 210 IWEW=1,NW
        IW = IWEW
        IF (EYEW .GT. AVEW) IW = NW+1-IWEW
        W = IW
C
C Load this slice of T into SLAB.
C
        IF (SVAL.EQ.0.) THEN
          DO 191 IU=1,NU
            DO 190 IV=1,NV
              SLAB(IU+1,IV+1) = T(IU,IV,IW)
  190       CONTINUE
  191     CONTINUE
        ELSE
          DO 193 IU=1,NU
            DO 192 IV=1,NV
              IF (T(IU,IV,IW).NE.SVAL) THEN
                SLAB(IU+1,IV+1) = T(IU,IV,IW)
              ELSE
                SLAB(IU+1,IV+1) = EDGE
              END IF
  192       CONTINUE
  193     CONTINUE
        END IF
C
C Contour this slab.
C
        CALL ISTRCL (SLAB,NUVWP2,NUP2,NVP2,TVAL)
C
C Construct the visibility array.
C
        CALL ISFILL
C
C If interpolation is turned on, fill in intermediate slabs.
C
        IF (NINW.NE.0.AND.IWEW.NE.NW) THEN
          IF (NINW.LT.0) ISDR=1
          IQ=IW+1
          IF (EYEW.GT.AVEW) IQ=IW-1
          DO 205 IINW=1,ABS(NINW)
            Q=REAL(IINW)/REAL(ABS(NINW)+1)
            P=1.-Q
            W=P*REAL(IW)+Q*REAL(IQ)
            IF (SVAL.EQ.0.) THEN
              DO 202 IU=1,NU
                DO 201 IV=1,NV
                  SLAB(IU+1,IV+1)=P*T(IU,IV,IW)+Q*T(IU,IV,IQ)
  201           CONTINUE
  202         CONTINUE
            ELSE
              DO 204 IU=1,NU
                DO 203 IV=1,NV
                  IF (T(IU,IV,IW).NE.SVAL.AND.T(IU,IV,IQ).NE.SVAL) THEN
                    SLAB(IU+1,IV+1)=P*T(IU,IV,IW)+Q*T(IU,IV,IQ)
                  ELSE
                    SLAB(IU+1,IV+1)=EDGE
                  END IF
  203           CONTINUE
  204         CONTINUE
            END IF
            CALL ISTRCL (SLAB,NUVWP2,NUP2,NVP2,TVAL)
            CALL ISFILL
  205     CONTINUE
          ISDR=0
        END IF
C
  210 CONTINUE
C
C Draw reference-plane edges and the W axis.
C
  220 IF (IREF .EQ. 0) RETURN
      CALL ISTR32 (SU1,SV1,SW1,XT,YT,DUM,2)
      IF (EYEV .LT. SV1) GO TO 240
      CALL ISPLTF (XT,YT,1)
      DO 230 IX=1,NX
        RU=1.+REAL(NU-1)*(REAL(IX)/REAL(NX))
        CALL ISTR32 (SU(RU),SV1,SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  230 CONTINUE
      GO TO 250
  240 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SUNU,SV1,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  250 IF (EYEU .GT. SUNU) GO TO 270
      CALL ISPLTF (XT,YT,1)
      DO 260 IX=1,NX
        RV=1.+REAL(NV-1)*(REAL(IX)/REAL(NX))
        CALL ISTR32 (SUNU,SV(RV),SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  260 CONTINUE
      GO TO 280
  270 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SUNU,SVNV,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  280 IF (EYEV .GT. SVNV) GO TO 300
      CALL ISPLTF (XT,YT,1)
      DO 290 IX=1,NX
        RU=1.+REAL(NU-1)*(REAL(NX-IX)/REAL(NX))
        CALL ISTR32 (SU(RU),SVNV,SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  290 CONTINUE
      GO TO 310
  300 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SU1,SVNV,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  310 IF (EYEU .LT. SU1) GO TO 330
      CALL ISPLTF (XT,YT,1)
      DO 320 IX=1,NX
        RV=1.+REAL(NV-1)*(REAL(NX-IX)/REAL(NX))
        CALL ISTR32 (SU1,SV(RV),SW1,XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  320 CONTINUE
      GO TO 340
  330 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SU1,SV1,SW1,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
  340 IF (EYEU.LE.SU1 .OR. EYEV.LE.SV1) GO TO 360
      CALL ISPLTF (XT,YT,1)
      DO 350 IX=1,NX
        RW=1.+REAL(NW-1)*(REAL(IX)/REAL(NX))
        CALL ISTR32 (SU1,SV1,SW(RW),XT,YT,DUM,2)
        CALL ISPLTF (XT,YT,2)
  350 CONTINUE
      RETURN
  360 CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,0)
      CALL ISTR32 (SU1,SV1,SWNW,XT,YT,DUM,2)
      CALL PLOTIF (XVPL+(XVPR-XVPL)*XT,YVPB+(YVPT-YVPB)*YT,1)
      RETURN
      END
