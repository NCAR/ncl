      SUBROUTINE GDSWIZCB(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                    LROT,CROT,SROT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  GDSWIZCB   GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
C           AND RETURNS ONE OF THE FOLLOWING:
C             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
C             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
C           FOR STAGGERED ROTATED EQUIDISTANT CYLINDRICAL PROJECTIONS.
C           (SEE UNDER THE DESCRIPTION OF KGDS TO DETERMINE WHETHER
C           TO COMPUTE A STAGGERED WIND GRID OR A STAGGERED MASS GRID.)
C           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
C           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
C           OUTPUT ELEMENTS ARE SET TO FILL VALUES.
C           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   98-08-19  BALDWIN    MODIFY GDSWIZC9 FOR TYPE 203 ETA GRIDS
C
C USAGE:    CALL GDSWIZCB(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C     &                   LROT,CROT,SROT)
C
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C                IMPORTANT NOTE: IF THE 9TH BIT (FROM RIGHT) OF KGDS(11)
C                                (SCANNING MODE FLAG) IS 1, THEN THIS
C                                THE GRID IS COMPUTED FOR A WIND FIELD;
C                                OTHERWISE IT IS FOR A MASS FIELD.  THUS
C                                MOD(KGDS(11)/256,2)=0 FOR MASS GRID.
C     IOPT     - INTEGER OPTION FLAG
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER KGDS(200)
      REAL XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
      REAL CROT(NPTS),SROT(NPTS)
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(KGDS(1).EQ.203) THEN
        RLAT1=KGDS(4)*1.E-3
        RLON1=KGDS(5)*1.E-3
        RLAT0=KGDS(7)*1.E-3
        RLON0=KGDS(8)*1.E-3
        IROT=MOD(KGDS(6)/8,2)
        IM=KGDS(2)*2-1
        JM=KGDS(3)
        KSCAN=MOD(KGDS(11)/256,2)
        ISCAN=MOD(KGDS(11)/128,2)
        JSCAN=MOD(KGDS(11)/64,2)
        NSCAN=MOD(KGDS(11)/32,2)
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        SLAT1=SIN(RLAT1/DPR)
        CLAT1=COS(RLAT1/DPR)
        SLAT0=SIN(RLAT0/DPR)
        CLAT0=COS(RLAT0/DPR)
        HS=SIGN(1.,MOD(RLON1-RLON0+180+3600,360.)-180)
        CLON1=COS((RLON1-RLON0)/DPR)
        SLATR=CLAT0*SLAT1-SLAT0*CLAT1*CLON1
        CLATR=SQRT(1-SLATR**2)
        CLONR=(CLAT0*CLAT1*CLON1+SLAT0*SLAT1)/CLATR
        RLATR=DPR*ASIN(SLATR)
        RLONR=HS*DPR*ACOS(CLONR)
        DLATS=RLATR/(-(JM-1)/2)
        DLONS=RLONR/(-(IM-1)/2)
        IF(KSCAN.EQ.0) THEN
          IS1=(JM+1)/2
        ELSE
          IS1=JM/2
        ENDIF
        XMIN=0
        XMAX=IM+1
        IF(IM.EQ.NINT(360/ABS(DLONS))) XMAX=IM+2
        YMIN=0
        YMAX=JM+1
        NRET=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
        IF(IOPT.EQ.0.OR.IOPT.EQ.1) THEN
          DO N=1,NPTS
            XPTF=YPTS(N)+(XPTS(N)-IS1)
            YPTF=YPTS(N)-(XPTS(N)-IS1)+KSCAN
            IF(XPTF.GE.XMIN.AND.XPTF.LE.XMAX.AND.
     &         YPTF.GE.YMIN.AND.YPTF.LE.YMAX) THEN
              HS=HI*SIGN(1.,XPTF-(IM+1)/2)
              RLONR=(XPTF-(IM+1)/2)*DLONS
              RLATR=(YPTF-(JM+1)/2)*DLATS
              CLONR=COS(RLONR/DPR)
              SLATR=SIN(RLATR/DPR)
              CLATR=COS(RLATR/DPR)
              SLAT=CLAT0*SLATR+SLAT0*CLATR*CLONR
              IF(SLAT.LE.-1) THEN
                CLAT=0.
                CLON=COS(RLON0/DPR)
                RLON(N)=0
                RLAT(N)=-90
              ELSEIF(SLAT.GE.1) THEN
                CLAT=0.
                CLON=COS(RLON0/DPR)
                RLON(N)=0
                RLAT(N)=90
              ELSE
                CLAT=SQRT(1-SLAT**2)
                CLON=(CLAT0*CLATR*CLONR-SLAT0*SLATR)/CLAT
                CLON=MIN(MAX(CLON,-1.),1.)
                RLON(N)=MOD(RLON0+HS*DPR*ACOS(CLON)+3600,360.)
                RLAT(N)=DPR*ASIN(SLAT)
              ENDIF
              NRET=NRET+1
              IF(LROT.EQ.1) THEN
                IF(IROT.EQ.1) THEN
                  IF(CLATR.LE.0) THEN
                    CROT(N)=-SIGN(1.,SLATR*SLAT0)
                    SROT(N)=0
                  ELSE
                    SLON=SIN((RLON(N)-RLON0)/DPR)
                    CROT(N)=(CLAT0*CLAT+SLAT0*SLAT*CLON)/CLATR
                    SROT(N)=SLAT0*SLON/CLATR
                  ENDIF
                ELSE
                  CROT(N)=1
                  SROT(N)=0
                ENDIF
              ENDIF
            ELSE
              RLON(N)=FILL
              RLAT(N)=FILL
            ENDIF
          ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
        ELSEIF(IOPT.EQ.-1) THEN
          DO N=1,NPTS
            IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90) THEN
              HS=SIGN(1.,MOD(RLON(N)-RLON0+180+3600,360.)-180)
              CLON=COS((RLON(N)-RLON0)/DPR)
              SLAT=SIN(RLAT(N)/DPR)
              CLAT=COS(RLAT(N)/DPR)
              SLATR=CLAT0*SLAT-SLAT0*CLAT*CLON
              IF(SLATR.LE.-1) THEN
                CLATR=0.
                RLONR=0
                RLATR=-90
              ELSEIF(SLATR.GE.1) THEN
                CLATR=0.
                RLONR=0
                RLATR=90
              ELSE
                CLATR=SQRT(1-SLATR**2)
                CLONR=(CLAT0*CLAT*CLON+SLAT0*SLAT)/CLATR
                CLONR=MIN(MAX(CLONR,-1.),1.)
                RLONR=HS*DPR*ACOS(CLONR)
                RLATR=DPR*ASIN(SLATR)
              ENDIF
              XPTF=(IM+1)/2+RLONR/DLONS
              YPTF=(JM+1)/2+RLATR/DLATS
              IF(XPTF.GE.XMIN.AND.XPTF.LE.XMAX.AND.
     &           YPTF.GE.YMIN.AND.YPTF.LE.YMAX) THEN
                XPTS(N)=IS1+(XPTF-(YPTF-KSCAN))/2
                YPTS(N)=(XPTF+(YPTF-KSCAN))/2
                NRET=NRET+1
                IF(LROT.EQ.1) THEN
                  IF(IROT.EQ.1) THEN
                    IF(CLATR.LE.0) THEN
                      CROT(N)=-SIGN(1.,SLATR*SLAT0)
                      SROT(N)=0
                    ELSE
                      SLON=SIN((RLON(N)-RLON0)/DPR)
                      CROT(N)=(CLAT0*CLAT+SLAT0*SLAT*CLON)/CLATR
                      SROT(N)=SLAT0*SLON/CLATR
                    ENDIF
                  ELSE
                    CROT(N)=1
                    SROT(N)=0
                  ENDIF
                ENDIF
              ELSE
                XPTS(N)=FILL
                YPTS(N)=FILL
              ENDIF
            ELSE
              XPTS(N)=FILL
              YPTS(N)=FILL
            ENDIF
          ENDDO
        ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTION UNRECOGNIZED
      ELSE
        IRET=-1
        IF(IOPT.GE.0) THEN
          DO N=1,NPTS
            RLON(N)=FILL
            RLAT(N)=FILL
          ENDDO
        ENDIF
        IF(IOPT.LE.0) THEN
          DO N=1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END

C-----------------------------------------------------------------------
      SUBROUTINE GDSWIZ(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                  LROT,CROT,SROT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  GDSWIZ     GRID DESCRIPTION SECTION WIZARD
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
C           AND RETURNS ONE OF THE FOLLOWING:
C             (IOPT= 0) GRID AND EARTH COORDINATES OF ALL GRID POINTS
C             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
C             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=201) STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=203) STAGGERED ROTATED EQUIDISTANT CYLINDRICAL 2-D
C           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
C           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
C           OUTPUT ELEMENTS ARE SET TO FILL VALUES.  ALSO IF IOPT=0,
C           IF THE NUMBER OF GRID POINTS EXCEEDS THE NUMBER ALLOTTED,
C           THEN ALL THE OUTPUT ELEMENTS ARE SET TO FILL VALUES.
C           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   98-08-20  BALDWIN  ADD TYPE 203 STAGGERED 2-D ETA GRIDS
C
C USAGE:    CALL GDSWIZ(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C     &                 LROT,CROT,SROT)
C
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     IOPT     - INTEGER OPTION FLAG
C                ( 0 TO COMPUTE EARTH COORDS OF ALL THE GRID POINTS)
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<=0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<=0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>=0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>=0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C                (-1 IF PROJECTION UNRECOGNIZED)
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C SUBPROGRAMS CALLED:
C   GDSWIZ00     GDS WIZARD FOR EQUIDISTANT CYLINDRICAL
C   GDSWIZ01     GDS WIZARD FOR MERCATOR CYLINDRICAL
C   GDSWIZ03     GDS WIZARD FOR LAMBERT CONFORMAL CONICAL
C   GDSWIZ04     GDS WIZARD FOR GAUSSIAN CYLINDRICAL
C   GDSWIZ05     GDS WIZARD FOR POLAR STEREOGRAPHIC AZIMUTHAL
C   GDSWIZC9     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL
C   GDSWIZCA     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL
C   GDSWIZCB     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL 2-D
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER KGDS(200)
      REAL XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
      REAL CROT(NPTS),SROT(NPTS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE GRID COORDINATES FOR ALL GRID POINTS
      IF(IOPT.EQ.0) THEN
        IF(KGDS(1).EQ.201) THEN
          IM=KGDS(7)*2-1
          JM=KGDS(8)
          KSCAN=MOD(KGDS(11)/256,2)
          IF(KSCAN.EQ.0) THEN
            IS1=(JM+1)/2
            NM=(IM/2+1)*JM-JM/2
          ELSE
            IS1=JM/2
            NM=IM/2*JM+JM/2
          ENDIF
        ELSEIF(KGDS(1).EQ.202) THEN
          IM=KGDS(7)
          JM=KGDS(8)
          NM=IM*JM
        ELSEIF(KGDS(1).EQ.203) THEN
          IM=KGDS(2)
          JM=KGDS(3)
          NM=IM*JM
          KSCAN=MOD(KGDS(11)/256,2)
          IF(KSCAN.EQ.0) THEN
            IS1=(JM+1)/2
          ELSE
            IS1=JM/2
          ENDIF
        ELSE
          IM=KGDS(2)
          JM=KGDS(3)
          NM=IM*JM
        ENDIF
        NSCAN=MOD(KGDS(11)/32,2)
        IF(NM.LE.NPTS) THEN
          IF(KGDS(1).EQ.201) THEN
            DO N=1,NM
              NN=2*N-1+KSCAN
              IF(NSCAN.EQ.0) THEN
                J=(NN-1)/IM+1
                I=NN-IM*(J-1)
              ELSE
                I=(NN-1)/JM+1
                J=NN-JM*(I-1)
              ENDIF
              XPTS(N)=IS1+(I-(J-KSCAN))/2
              YPTS(N)=(I+(J-KSCAN))/2
            ENDDO
          ELSEIF(KGDS(1).EQ.203) THEN
            DO N=1,NM
              IF(NSCAN.EQ.0) THEN
                J=(N-1)/IM+1
                I=(N-IM*(J-1))*2-MOD(J+KSCAN,2)
              ELSE
                I=(N-1)/JM+1
                J=(N-JM*(I-1))*2-MOD(I+KSCAN,2)
              ENDIF
              XPTS(N)=IS1+(I-(J-KSCAN))/2
              YPTS(N)=(I+(J-KSCAN))/2
            ENDDO
          ELSE
            DO N=1,NM
              IF(NSCAN.EQ.0) THEN
                J=(N-1)/IM+1
                I=N-IM*(J-1)
              ELSE
                I=(N-1)/JM+1
                J=N-JM*(I-1)
              ENDIF
              XPTS(N)=I
              YPTS(N)=J
            ENDDO
          ENDIF
          DO N=NM+1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ELSE
          DO N=1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ENDIF
        IOPF=1
      ELSE
        IOPF=IOPT
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  EQUIDISTANT CYLINDRICAL
c      IF(KGDS(1).EQ.000) THEN
c        CALL GDSWIZ00(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
cC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  MERCATOR CYLINDRICAL
c      ELSEIF(KGDS(1).EQ.001) THEN
c        CALL GDSWIZ01(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
cC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  LAMBERT CONFORMAL CONICAL
c      ELSEIF(KGDS(1).EQ.003) THEN
c        CALL GDSWIZ03(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
cC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  GAUSSIAN CYLINDRICAL
c      ELSEIF(KGDS(1).EQ.004) THEN
c        CALL GDSWIZ04(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
cC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  POLAR STEREOGRAPHIC AZIMUTHAL
c      ELSEIF(KGDS(1).EQ.005) THEN
c        CALL GDSWIZ05(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
cC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
c      ELSEIF(KGDS(1).EQ.201) THEN
c        CALL GDSWIZC9(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
cC - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cC  ROTATED EQUIDISTANT CYLINDRICAL
c      ELSEIF(KGDS(1).EQ.202) THEN
c        CALL GDSWIZCA(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
c     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
      IF(KGDS(1).EQ.203) THEN
        CALL GDSWIZCB(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTION UNRECOGNIZED
      ELSE
        IRET=-1
        IF(IOPT.GE.0) THEN
          DO N=1,NPTS
            RLON(N)=FILL
            RLAT(N)=FILL
          ENDDO
        ENDIF
        IF(IOPT.LE.0) THEN
          DO N=1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
