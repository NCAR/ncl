C
C $Id: mapchi.f,v 1.4 1994-03-18 23:49:50 kennison Exp $
C
      SUBROUTINE MAPCHI (IPRT,IDTG,IDPT)
C
C MAPCHI is called by various EZMAP routines to reset the color index,
C dotting, and dash pattern before and after drawing parts of a map.
C
C The argument IPRT, if positive, says which part of the map is about
C to be drawn, as follows:
C
C     IPRT    Part of map.
C     ----    ------------
C       1     Perimeter.
C       2     Grid.
C       3     Labelling.
C       4     Limb lines.
C       5     Outline point group, continental.
C       6     Outline point group, U.S.
C       7     Outline point group, country.
C
C A call with IPRT equal to the negative of one of these values asks
C that the color index saved by the last call, with IPRT positive, be
C restored.
C
C When IPRT is positive, IDTG is zero if solid lines are to be used, 1
C if dotted lines are to be used.  If IPRT is negative, IDTG is ignored.
C
C When IPRT is positive and IDTG is zero, IDPT is the dash pattern to be
C used.  If IPRT is negative or IDTG is non-zero, IDPT is ignored.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMQ/ ICIN(7)
      SAVE /MAPCMQ/
C
C Declare one of the dash-package common blocks, too.
C
      COMMON /SMFLAG/ ISMO
      SAVE /SMFLAG/
C
C Certain variables need to be saved between calls.
C
      SAVE IDTS,IPLS,IPMS,ISMS,ITXS
C
C Flush all buffers before changing anything.
C
      CALL MAPIQ
      IF (ICFELL('MAPCHI',1).NE.0) RETURN
C
C Set/reset color index, dotting, and dash pattern.  The user has the
C last word.
C
      IF (IPRT.GT.0) THEN
        ISMS=ISMO
        ISMO=1
        IDTS=IDTL
        IDTL=IDTG
        IF (IDTL.EQ.0) THEN
          CALL DASHDB (IDPT)
          IF (ICFELL('MAPCHI',2).NE.0) THEN
            IIER=-1
            RETURN
          END IF
        END IF
        IF (ICIN(IPRT).GE.0) THEN
          CALL GQPLCI (IGER,IPLS)
          IF (IGER.NE.0) THEN
            IIER=-1
            CALL SETER ('MAPCHI - ERROR EXIT FROM GQPLCI',IIER,1)
            RETURN
          END IF
          CALL GQPMCI (IGER,IPMS)
          IF (IGER.NE.0) THEN
            IIER=-1
            CALL SETER ('MAPCHI - ERROR EXIT FROM GQPMCI',IIER,1)
            RETURN
          END IF
          CALL GQTXCI (IGER,ITXS)
          IF (IGER.NE.0) THEN
            IIER=-1
            CALL SETER ('MAPCHI - ERROR EXIT FROM GQTXCI',IIER,1)
            RETURN
          END IF
          CALL GSPLCI (ICIN(IPRT))
          CALL GSPMCI (ICIN(IPRT))
          CALL GSTXCI (ICIN(IPRT))
        END IF
        CALL MAPUSR (IPRT)
        IF (ICFELL('MAPCHI',3).NE.0) RETURN
      ELSE
        CALL MAPUSR (IPRT)
        IF (ICFELL('MAPCHI',4).NE.0) RETURN
        IF (ICIN(-IPRT).GE.0) THEN
          CALL GSPLCI (IPLS)
          CALL GSPMCI (IPMS)
          CALL GSTXCI (ITXS)
        END IF
        IF (IDTL.EQ.0) THEN
          CALL DASHDB (IOR(ISHIFT(32767,1),1))
          IF (ICFELL('MAPCHI',5).NE.0) THEN
            IIER=-1
            RETURN
          END IF
        END IF
        IDTL=IDTS
        ISMO=ISMS
      END IF
C
C Done.
C
      RETURN
C
      END
