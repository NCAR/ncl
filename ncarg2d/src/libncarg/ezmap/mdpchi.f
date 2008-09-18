C
C $Id: mdpchi.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPCHI (IPRT,IDTG,IDPT)
C
        INTEGER IPRT,IDTG,IDPT
C
C MDPCHI is called by various EZMAP routines to reset the color index,
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
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
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
        COMMON /MAPCMQ/  ICIN(8)
        INTEGER          ICIN
        SAVE   /MAPCMQ/
C
C Declare one of the dash-package common blocks, too.
C
        COMMON /SMFLAG/  ISMO
        INTEGER          ISMO
        SAVE   /SMFLAG/
C
C Declare local variables.
C
        INTEGER          IDTS,IGER,IPLS,IPMS,ISMS,ITXS
C
C Certain variables need to be saved between calls.
C
        SAVE IDTS,IPLS,IPMS,ISMS,ITXS
C
C Flush all buffers before changing anything.
C
        CALL MDPIQ
        IF (ICFELL('MDPCHI',1).NE.0) RETURN
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
            IF (ICFELL('MDPCHI',2).NE.0) RETURN
          END IF
          IF (ICIN(IPRT).GE.0) THEN
            CALL GQPLCI (IGER,IPLS)
            IF (IGER.NE.0) THEN
              CALL SETER ('MDPCHI - ERROR EXIT FROM GQPLCI',3,1)
              RETURN
            END IF
            CALL GQPMCI (IGER,IPMS)
            IF (IGER.NE.0) THEN
              CALL SETER ('MDPCHI - ERROR EXIT FROM GQPMCI',4,1)
              RETURN
            END IF
            CALL GQTXCI (IGER,ITXS)
            IF (IGER.NE.0) THEN
              CALL SETER ('MDPCHI - ERROR EXIT FROM GQTXCI',5,1)
              RETURN
            END IF
            CALL GSPLCI (ICIN(IPRT))
            CALL GSPMCI (ICIN(IPRT))
            CALL GSTXCI (ICIN(IPRT))
          END IF
          CALL HLUMAPUSR (IPRT)
          IF (ICFELL('MDPCHI',6).NE.0) RETURN
        ELSE
          CALL HLUMAPUSR (IPRT)
          IF (ICFELL('MDPCHI',7).NE.0) RETURN
          IF (ICIN(-IPRT).GE.0) THEN
            CALL GSPLCI (IPLS)
            CALL GSPMCI (IPMS)
            CALL GSTXCI (ITXS)
          END IF
          IF (IDTL.EQ.0) THEN
            CALL DASHDB (IOR(ISHIFT(32767,1),1))
            IF (ICFELL('MDPCHI',8).NE.0) RETURN
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
