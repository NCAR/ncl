C
C $Id: mdrgsf.f,v 1.12 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGSF (IRGL,RWRK,LRWK,IAMA,LAMA)
C
        INTEGER IRGL,LRWK,IAMA(LAMA),LAMA
        REAL RWRK(LRWK)
C
C This routine is called to draw RANGS/GSHHS polygons in the current
C EZMAP window.  The argument IRGL specifies the level of resolution
C to be used, from 0 (highest resolution) to 4 (lowest resolution).
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
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
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C Declare local variables.
C
        DOUBLE PRECISION P,Q,RLAT,RLON,UPRJ,VPRJ
        INTEGER          I,J,ICAT,ICEL,IERR,IFAC,IFUA,IGCF,ILAT,ILON,
     +                   IRIM,IWGF,NPTS
        REAL             DUMI(4)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRGSF - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDRGSF',2).NE.0) RETURN
        END IF
C
C Open the RANGS/GSHHS files.
C
        CALL MDRGOF (MAX(0,MIN(4,IRGL)),ICAT,ICEL,IRIM)
        IF (ICFELL('MDRGSF',3).NE.0) RETURN
C
C Save the current state of the GKS clipping flag and turn clipping on.
C (Note that the call to GTX to write a blank is a work-around that has
C no visible effect on the picture but causes GKS to send the clipping
C flag and clipping rectangle to a workstation even if nothing else had
C previously been sent to it, so don't take it out unless you know that
C the behavior of GKS in this regard has been changed.)
C
        CALL GQCLIP (IERR,IGCF,DUMI)
C
        IF (IERR.NE.0) THEN
          CALL SETER ('MDRGSF - ERROR EXIT FROM GQCLIP',4,1)
          RETURN
        END IF
C
        CALL GTX    (CFUX(.5),CFUY(.5),' ')
C
        CALL GSCLIP (1)
C
C Save the current fill-area color.
C
        CALL GQFACI (IERR,IFAC)
C
        IF (IERR.NE.0) THEN
          CALL SETER ('MDRGSF - ERROR EXIT FROM GQFACI',5,1)
          RETURN
        END IF
C
C Decide whether to do the fill using MDRGSQ, which fills the polygons
C directly, or using MDRGSX, which uses an area map.  The latter is done
C only when the filling of the polygons at some nesting level is to be
C omitted, but the filling of those at a higher level was not.
C
        IFUA=-1
C
        DO 101 I=1,5
          IF (ICSF(I).GE.0) THEN
            IFUA=0
          ELSE
            IF (IFUA.EQ.0) THEN
              IFUA=2
              GO TO 102
            END IF
          END IF
  101   CONTINUE
C
        IF (IFUA.LT.0) GO TO 108
C
C Look for 1-degree lat/lon "squares" that are visible and plot
C polygons in them.
C
  102   NPTS=7
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each 1-degree square for intersection with
C the window.
C
        IWGF=0
        IF (BLAM-SLAM.GT.179.D0.AND.BLOM-SLOM.GT.359.D0) IWGF=1
C
C Loop through all the 1-degree lat/lon squares; for each one, do a
C quick test to see if the square is entirely invisible and, if not,
C do more exhaustive tests to see if part of it is visible.
C
        DO 107 ILON=0,359
          IF (IWGF.EQ.0.AND.
     +        (DBLE(ILON   ).GT.BLOM.OR.
     +         DBLE(ILON+1 ).LT.SLOM).AND.
     +        (DBLE(ILON-360).GT.BLOM.OR.
     +         DBLE(ILON-359).LT.SLOM).AND.
     +        (DBLE(ILON+360).GT.BLOM.OR.
     +         DBLE(ILON+361).LT.SLOM))                        GO TO 107
          DO 106 ILAT = -90,89
            IF (IWGF.EQ.0.AND.
     +          (DBLE(ILAT).GT.BLAM.OR.DBLE(ILAT+1).LT.SLAM))  GO TO 106
            DO 104 I=1,NPTS
              P=DBLE(I-1)/DBLE(NPTS-1)
              DO 103 J=1,NPTS
                Q=DBLE(J-1)/DBLE(NPTS-1)
                CALL MDPTRA (DBLE(ILAT)+P,DBLE(ILON)+Q,UPRJ,VPRJ)
                IF (UPRJ.NE.1.D12)                             GO TO 105
                CALL MDPTRI (UMIN+P*(UMAX-UMIN),VMIN+Q*(VMAX-VMIN),
     +                                                   RLAT,RLON)
                IF (RLON.LT.DBLE(ILON)) RLON=RLON+360.D0
                IF (RLON.GT.DBLE(ILON+360)) RLON=RLON-360.D0
                IF (RLAT.GE.DBLE(ILAT).AND.RLAT.LE.DBLE(ILAT+1).AND.
     +              RLON.GE.DBLE(ILON).AND.RLON.LE.DBLE(ILON+1))
     +                                                         GO TO 105
  103         CONTINUE
  104       CONTINUE
                                                               GO TO 106
  105       IF (IDPF.EQ.0.AND.IFUA.EQ.0) THEN
              CALL MDRGSQ (ICAT,ICEL,IRIM,ILAT,ILON,1,
     +                                       RWRK,RWRK(LRWK/2+1),LRWK/2)
            ELSE
              CALL MDRGSX (ICAT,ICEL,IRIM,ILAT,ILON,
     +                                     MAX(1,MIN(2,IDPF+IFUA)),IRGL,
     +                             IAMA,LAMA,RWRK,RWRK(LRWK/2+1),LRWK/2)
            END IF
            IF (ICFELL('MDRGSF',6).NE.0)                       GO TO 108
  106     CONTINUE
  107   CONTINUE
C
C Restore the fill-area color to its original value.
C
  108   CALL GSFACI (IFAC)
C
C Reset the clipping flag.
C
        CALL GSCLIP (IGCF)
C
C Close all RANGS/GSHHS files.
C
        CALL NGCLFI (ICAT)
        CALL NGCLFI (ICEL)
        CALL NGCLFI (IRIM)
C
C Done.
C
        RETURN
C
      END
