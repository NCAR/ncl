C
C $Id: mdrgol.f,v 1.12 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGOL (IRGL,RWRK,LRWK)
C
        INTEGER IRGL,LRWK
        REAL RWRK(LRWK)
C
C This routine is called to draw RANGS/GSHHS outlines in the current
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
        INTEGER          I,J,ICAT,ICEL,IERR,IGCF,ILAT,ILON,IPLC,IRIM,
     +                   IWGF,NPTS
        REAL             DUMI(4)
C
C Declare a local, dummy, area-map array to hand off to MDRGSX, which
C doesn't need a real one just to draw outlines.
C
        INTEGER          IAMA(1)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRGOL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDRGOL',2).NE.0) RETURN
        END IF
C
C Open the RANGS/GSHHS files.
C
        CALL MDRGOF (MAX(0,MIN(4,IRGL)),ICAT,ICEL,IRIM)
        IF (ICFELL('MDRGOL',3).NE.0) RETURN
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
          CALL SETER ('MDRGOL - ERROR EXIT FROM GQCLIP',4,1)
          RETURN
        END IF
C
        CALL GTX    (CFUX(.5),CFUY(.5),' ')
C
        CALL GSCLIP (1)
C
C Save the current polyline color.
C
        CALL GQPLCI (IERR,IPLC)
C
        IF (IERR.NE.0) THEN
          CALL SETER ('MDRGOL - ERROR EXIT FROM GQPLCI',5,1)
          RETURN
        END IF
C
C Look for 1-degree lat/lon "squares" that are visible and plot
C polylines in them.
C
        NPTS=7
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
        DO 105 ILON=0,359
          IF (IWGF.EQ.0.AND.
     +        (DBLE(ILON   ).GT.BLOM.OR.
     +         DBLE(ILON+1 ).LT.SLOM).AND.
     +        (DBLE(ILON-360).GT.BLOM.OR.
     +         DBLE(ILON-359).LT.SLOM).AND.
     +        (DBLE(ILON+360).GT.BLOM.OR.
     +         DBLE(ILON+361).LT.SLOM))                        GO TO 105
          DO 104 ILAT = -90,89
            IF (IWGF.EQ.0.AND.
     +          (DBLE(ILAT).GT.BLAM.OR.DBLE(ILAT+1).LT.SLAM))  GO TO 104
            DO 102 I=1,NPTS
              P=DBLE(I-1)/DBLE(NPTS-1)
              DO 101 J=1,NPTS
                Q=DBLE(J-1)/DBLE(NPTS-1)
                CALL MDPTRA (DBLE(ILAT)+P,DBLE(ILON)+Q,UPRJ,VPRJ)
                IF (UPRJ.NE.1.D12)                             GO TO 103
                CALL MDPTRI (UMIN+P*(UMAX-UMIN),VMIN+Q*(VMAX-VMIN),
     +                                                   RLAT,RLON)
                IF (RLON.LT.DBLE(ILON)) RLON=RLON+360.D0
                IF (RLON.GT.DBLE(ILON+360)) RLON=RLON-360.D0
                IF (RLAT.GE.DBLE(ILAT).AND.RLAT.LE.DBLE(ILAT+1).AND.
     +              RLON.GE.DBLE(ILON).AND.RLON.LE.DBLE(ILON+1))
     +                                                         GO TO 103
  101         CONTINUE
  102       CONTINUE
                                                               GO TO 104
  103       IF (IDPF.EQ.0) THEN
              CALL MDRGSQ (ICAT,ICEL,IRIM,ILAT,ILON,0,
     +                                       RWRK,RWRK(LRWK/2+1),LRWK/2)
            ELSE
              CALL MDRGSX (ICAT,ICEL,IRIM,ILAT,ILON,0,IRGL,
     +                                IAMA,1,RWRK,RWRK(LRWK/2+1),LRWK/2)
            END IF
            IF (ICFELL('MDRGOL',6).NE.0)                       GO TO 106
  104     CONTINUE
  105   CONTINUE
C
C Restore the polyline color to its original value.
C
  106   CALL GSPLCI (IPLC)
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
