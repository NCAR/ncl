C
C $Id: maplbl.f,v 1.8 1999-04-02 22:59:35 kennison Exp $
C
      SUBROUTINE MAPLBL
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UCEN,VCEN,URNG,VRNG,BLAM,SLAM,
     +                BLOM,SLOM,ISSL,PEPS
      SAVE   /MAPCM2/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE   /MAPCMA/
C
C Declare a character temporary to use.
C
      CHARACTER*4 CTMP
C
C Define required constants.  SIN1 and COS1 are respectively the sine
C and cosine of one degree.
C
      DATA SIN1 / .017452406437283 /
      DATA COS1 / .999847695156390 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPLBL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPLBL',2).NE.0) RETURN
      END IF
C
C If requested, letter key meridians and poles.
C
      IF (LBLF) THEN
C
C Reset the color index, dotting, and dash pattern for labelling.
C
        CALL MAPCHI (3,1,0)
        IF (ICFELL('MAPLBL',3).NE.0) RETURN
C
C First, the North pole.
C
        CALL MAPTRA (90.,0.,U,V)
        IF (ICFELL('MAPLBL',4).NE.0) RETURN
        IF (U.LT.1.E12) THEN
          CALL WTSTR (U,V,'NP',ILCW,0,0)
          IF (ICFELL('MAPLBL',5).NE.0) RETURN
        END IF
C
C Then, the South pole.
C
        CALL MAPTRA (-90.,0.,U,V)
        IF (ICFELL('MAPLBL',6).NE.0) RETURN
        IF (U.LT.1.E12) THEN
          CALL WTSTR (U,V,'SP',ILCW,0,0)
          IF (ICFELL('MAPLBL',7).NE.0) RETURN
        END IF
C
C The equator.
C
        RLON=PHOC-10.
C
        DO 101 ILON=1,36
          RLON=RLON+10.
          CALL MAPTRA (0.,RLON,U,V)
          IF (ICFELL('MAPLBL',8).NE.0) RETURN
          IF (U.LT.1.E12) THEN
            CALL WTSTR (U,V,'EQ',ILCW,0,0)
            IF (ICFELL('MAPLBL',9).NE.0) RETURN
            GO TO 102
          END IF
  101   CONTINUE
C
C The Greenwich meridian.
C
  102   DO 103 ILAT=75,-75,-10
          RLAT=REAL(ILAT)
          CALL MAPTRA (RLAT,0.,U,V)
          IF (ICFELL('MAPLBL',10).NE.0) RETURN
          IF (U.LT.1.E12) THEN
            CALL WTSTR (U,V,'GM',ILCW,0,0)
            IF (ICFELL('MAPLBL',11).NE.0) RETURN
            GO TO 104
          END IF
  103   CONTINUE
C
C International date line.
C
  104   DO 105 ILAT=75,-75,-10
          RLAT=REAL(ILAT)
          CALL MAPTRA (RLAT,180.,U,V)
          IF (ICFELL('MAPLBL',12).NE.0) RETURN
          IF (U.LT.1.E12) THEN
            CALL WTSTR (U,V,'ID',ILCW,0,0)
            IF (ICFELL('MAPLBL',13).NE.0) RETURN
            GO TO 106
          END IF
  105   CONTINUE
C
C Restore the color index, dotting, and dash pattern.
C
  106   CALL MAPCHI (-3,0,0)
        IF (ICFELL('MAPLBL',14).NE.0) RETURN
C
      END IF
C
C Draw perimeter, if requested.
C
      IF (PRMF) THEN
C
C Reset the color index, dotting, and dash pattern for the perimeter.
C
        CALL MAPCHI (1,0,IOR(ISHIFT(32767,1),1))
        IF (ICFELL('MAPLBL',15).NE.0) RETURN
C
C The perimeter is either an ellipse or a rectangle, depending on ELPF.
C
        IF (ELPF) THEN
          U=.9999*URNG
          V=0.
          IF (IDTL.EQ.0) THEN
            CALL FRSTD (UCEN+U,VCEN)
            IF (ICFELL('MAPLBL',16).NE.0) RETURN
          ELSE
            DATL=0.
          END IF
          DO 110 I=1,360
            UOLD=U
            VOLD=V
            U=COS1*UOLD-SIN1*VOLD
            V=SIN1*UOLD+COS1*VOLD
            CALL MAPVP (UCEN+UOLD,VCEN+VOLD*VRNG/URNG,
     +                  UCEN+U   ,VCEN+V   *VRNG/URNG)
            IF (ICFELL('MAPLBL',17).NE.0) RETURN
  110     CONTINUE
        ELSE
          UMINX=UMIN+.9999*(UMAX-UMIN)
          UMAXX=UMAX-.9999*(UMAX-UMIN)
          VMINX=VMIN+.9999*(VMAX-VMIN)
          VMAXX=VMAX-.9999*(VMAX-VMIN)
          IF (IDTL.EQ.0) THEN
            CALL FRSTD (UMINX,VMINX)
            IF (ICFELL('MAPLBL',18).NE.0) RETURN
          ELSE
            DATL=0.
          END IF
          CALL MAPVP (UMINX,VMINX,UMAXX,VMINX)
          IF (ICFELL('MAPLBL',19).NE.0) RETURN
          CALL MAPVP (UMAXX,VMINX,UMAXX,VMAXX)
          IF (ICFELL('MAPLBL',20).NE.0) RETURN
          CALL MAPVP (UMAXX,VMAXX,UMINX,VMAXX)
          IF (ICFELL('MAPLBL',21).NE.0) RETURN
          CALL MAPVP (UMINX,VMAXX,UMINX,VMINX)
          IF (ICFELL('MAPLBL',22).NE.0) RETURN
        END IF
C
C Restore the color index, dotting, and dash pattern.
C
        CALL MAPCHI (-1,0,0)
        IF (ICFELL('MAPLBL',23).NE.0) RETURN
C
      END IF
C
C Done.
C
      RETURN
C
      END
