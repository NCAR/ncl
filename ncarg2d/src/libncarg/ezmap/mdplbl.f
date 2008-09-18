C
C $Id: mdplbl.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPLBL
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
C Declare local variables.
C
        DOUBLE PRECISION RLAT,RLON,U,UMAXX,UMINX,UOLD,V,VMAXX,VMINX,VOLD
        INTEGER          I,ILAT,ILON
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPLBL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPLBL',2).NE.0) RETURN
        END IF
C
C If requested, letter key meridians and poles.
C
        IF (LBLF) THEN
C
C Reset the color index, dotting, and dash pattern for labelling.
C
          CALL MDPCHI (3,1,0)
          IF (ICFELL('MDPLBL',3).NE.0) RETURN
C
C First, the North pole.
C
          CALL MDPTRA (90.D0,0.D0,U,V)
          IF (ICFELL('MDPLBL',4).NE.0) RETURN
          IF (U.LT.1.D12) THEN
            CALL WTSTR (REAL(U),REAL(V),'NP',ILCW,0,0)
            IF (ICFELL('MDPLBL',5).NE.0) RETURN
          END IF
C
C Then, the South pole.
C
          CALL MDPTRA (-90.D0,0.D0,U,V)
          IF (ICFELL('MDPLBL',6).NE.0) RETURN
          IF (U.LT.1.D12) THEN
            CALL WTSTR (REAL(U),REAL(V),'SP',ILCW,0,0)
            IF (ICFELL('MDPLBL',7).NE.0) RETURN
          END IF
C
C The equator.
C
          RLON=PLNO-10.D0
C
          DO 101 ILON=1,36
            RLON=RLON+10.D0
            CALL MDPTRA (0.D0,RLON,U,V)
            IF (ICFELL('MDPLBL',8).NE.0) RETURN
            IF (U.LT.1.D12) THEN
              CALL WTSTR (REAL(U),REAL(V),'EQ',ILCW,0,0)
              IF (ICFELL('MDPLBL',9).NE.0) RETURN
              GO TO 102
            END IF
  101     CONTINUE
C
C The Greenwich meridian.
C
  102     DO 103 ILAT=75,-75,-10
            RLAT=DBLE(ILAT)
            CALL MDPTRA (RLAT,0.D0,U,V)
            IF (ICFELL('MDPLBL',10).NE.0) RETURN
            IF (U.LT.1.D12) THEN
              CALL WTSTR (REAL(U),REAL(V),'GM',ILCW,0,0)
              IF (ICFELL('MDPLBL',11).NE.0) RETURN
              GO TO 104
            END IF
  103     CONTINUE
C
C International date line.
C
  104     DO 105 ILAT=75,-75,-10
            RLAT=DBLE(ILAT)
            CALL MDPTRA (RLAT,180.D0,U,V)
            IF (ICFELL('MDPLBL',12).NE.0) RETURN
            IF (U.LT.1.D12) THEN
              CALL WTSTR (REAL(U),REAL(V),'ID',ILCW,0,0)
              IF (ICFELL('MDPLBL',13).NE.0) RETURN
              GO TO 106
            END IF
  105     CONTINUE
C
C Restore the color index, dotting, and dash pattern.
C
  106     CALL MDPCHI (-3,0,0)
          IF (ICFELL('MDPLBL',14).NE.0) RETURN
C
        END IF
C
C Draw perimeter, if requested.
C
        IF (PRMF) THEN
C
C Reset the color index, dotting, and dash pattern for the perimeter.
C
          CALL MDPCHI (1,0,IOR(ISHIFT(32767,1),1))
          IF (ICFELL('MDPLBL',15).NE.0) RETURN
C
C The perimeter is either an ellipse or a rectangle, depending on ELPF.
C
          IF (ELPF) THEN
            U=.9999D0*URNG
            V=0.D0
            IF (IDTL.EQ.0) THEN
              CALL FRSTD (REAL(UCEN+U),REAL(VCEN))
              IF (ICFELL('MDPLBL',16).NE.0) RETURN
            ELSE
              DATL=0.D0
            END IF
            DO 110 I=1,360
              UOLD=U
              VOLD=V
              U=COS1*UOLD-SIN1*VOLD
              V=SIN1*UOLD+COS1*VOLD
              CALL MDPVP (UCEN+UOLD,VCEN+VOLD*VRNG/URNG,
     +                    UCEN+U   ,VCEN+V   *VRNG/URNG)
              IF (ICFELL('MDPLBL',17).NE.0) RETURN
  110       CONTINUE
          ELSE
            UMINX=UMIN+.9999D0*(UMAX-UMIN)
            UMAXX=UMAX-.9999D0*(UMAX-UMIN)
            VMINX=VMIN+.9999D0*(VMAX-VMIN)
            VMAXX=VMAX-.9999D0*(VMAX-VMIN)
            IF (IDTL.EQ.0) THEN
              CALL FRSTD (REAL(UMINX),REAL(VMINX))
              IF (ICFELL('MDPLBL',18).NE.0) RETURN
            ELSE
              DATL=0.D0
            END IF
            CALL MDPVP (UMINX,VMINX,UMAXX,VMINX)
            IF (ICFELL('MDPLBL',19).NE.0) RETURN
            CALL MDPVP (UMAXX,VMINX,UMAXX,VMAXX)
            IF (ICFELL('MDPLBL',20).NE.0) RETURN
            CALL MDPVP (UMAXX,VMAXX,UMINX,VMAXX)
            IF (ICFELL('MDPLBL',21).NE.0) RETURN
            CALL MDPVP (UMINX,VMAXX,UMINX,VMINX)
            IF (ICFELL('MDPLBL',22).NE.0) RETURN
          END IF
C
C Restore the color index, dotting, and dash pattern.
C
          CALL MDPCHI (-1,0,0)
          IF (ICFELL('MDPLBL',23).NE.0) RETURN
C
        END IF
C
C Done.
C
        RETURN
C
      END
