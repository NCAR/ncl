C
C $Id: maplbl.f,v 1.7 1998-05-23 20:19:47 kennison Exp $
C
      SUBROUTINE MAPLBL
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE /MAPCM1/
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
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
C If EZMAP needs initialization, do nothing.
C
      IF (INTF) RETURN
C
C If requested, letter key meridians and poles.
C
      IF (LBLF) THEN
C
C Reset the color index, dotting, and dash pattern for labelling.
C
        CALL MAPCHI (3,1,0)
        IF (ICFELL('MAPLBL',2).NE.0) RETURN
C
C First, the North pole.
C
        CALL MAPTRN (90.,0.,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                                   .AND.V.LE.VMAX)
     +     .OR.(ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                    CALL WTSTR (U,V,'NP',ILCW,0,0)
C
C Then, the South pole.
C
        CALL MAPTRN (-90.,0.,U,V)
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                                   .AND.V.LE.VMAX)
     +     .OR.(ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                    CALL WTSTR (U,V,'SP',ILCW,0,0)
C
C The equator.
C
        RLON=PHOC-10.
        DO 101 I=1,36
          RLON=RLON+10.
          CALL MAPTRN (0.,RLON,U,V)
          IF (ICFELL('MAPLBL',3).NE.0) RETURN
          IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                                   .AND.V.LE.VMAX)
     +     .OR.(ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 102
  101   CONTINUE
        GO TO 103
  102   CALL WTSTR (U,V,'EQ',ILCW,0,0)
        IF (ICFELL('MAPLBL',4).NE.0) RETURN
C
C The Greenwich meridian.
C
  103   RLAT=85.
        DO 104 I=1,16
          RLAT=RLAT-10.
          CALL MAPTRN (RLAT,0.,U,V)
          IF (ICFELL('MAPLBL',5).NE.0) RETURN
          IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                                   .AND.V.LE.VMAX)
     +     .OR.(ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 105
  104   CONTINUE
        GO TO 106
  105   CALL WTSTR (U,V,'GM',ILCW,0,0)
        IF (ICFELL('MAPLBL',6).NE.0) RETURN
C
C International date line.
C
  106   RLAT=85.
        DO 107 I=1,16
          RLAT=RLAT-10.
          CALL MAPTRN (RLAT,180.,U,V)
          IF (ICFELL('MAPLBL',7).NE.0) RETURN
          IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                                   .AND.V.LE.VMAX)
     +     .OR.(ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 108
  107   CONTINUE
        GO TO 109
  108   CALL WTSTR (U,V,'ID',ILCW,0,0)
        IF (ICFELL('MAPLBL',8).NE.0) RETURN
C
C Restore the color index, dotting, and dash pattern.
C
  109   CALL MAPCHI (-3,0,0)
        IF (ICFELL('MAPLBL',9).NE.0) RETURN
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
        IF (ICFELL('MAPLBL',10).NE.0) RETURN
C
C The perimeter is either an ellipse or a rectangle, depending on ELPF.
C
        IF (ELPF) THEN
          U=.9999*URNG
          V=0.
          IF (IDTL.EQ.0) THEN
            CALL FRSTD (UCEN+U,VCEN)
            IF (ICFELL('MAPLBL',11).NE.0) RETURN
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
            IF (ICFELL('MAPLBL',12).NE.0) RETURN
  110     CONTINUE
        ELSE
          UMINX=UMIN+.9999*(UMAX-UMIN)
          UMAXX=UMAX-.9999*(UMAX-UMIN)
          VMINX=VMIN+.9999*(VMAX-VMIN)
          VMAXX=VMAX-.9999*(VMAX-VMIN)
          IF (IDTL.EQ.0) THEN
            CALL FRSTD (UMINX,VMINX)
            IF (ICFELL('MAPLBL',13).NE.0) RETURN
          ELSE
            DATL=0.
          END IF
          CALL MAPVP (UMINX,VMINX,UMAXX,VMINX)
          IF (ICFELL('MAPLBL',14).NE.0) RETURN
          CALL MAPVP (UMAXX,VMINX,UMAXX,VMAXX)
          IF (ICFELL('MAPLBL',15).NE.0) RETURN
          CALL MAPVP (UMAXX,VMAXX,UMINX,VMAXX)
          IF (ICFELL('MAPLBL',16).NE.0) RETURN
          CALL MAPVP (UMINX,VMAXX,UMINX,VMINX)
          IF (ICFELL('MAPLBL',17).NE.0) RETURN
        END IF
C
C Restore the color index, dotting, and dash pattern.
C
        CALL MAPCHI (-1,0,0)
        IF (ICFELL('MAPLBL',18).NE.0) RETURN
C
      END IF
C
C Done.
C
      RETURN
C
      END
