C
C	$Id: maplot.f,v 1.1.1.1 1992-04-17 22:32:05 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPLOT
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE /MAPCM3/
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
C Define required constants.
C
      DATA PI / 3.14159265358979 /
C
C If EZMAP needs initialization or if an error has occurred since the
C last initialization, do nothing.
C
      IF (INTF) RETURN
      IF (IIER.NE.0) RETURN
C
C If the selected outline type is "NONE", do nothing.
C
      IF (NOUT.LE.0) RETURN
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window.
C
      IWGF=0
      IF (BLAM-SLAM.GT.179.9999.AND.BLOM-SLOM.GT.359.9999) IWGF=1
C
C IGIS keeps track of changes in the group identifier, so that the
C color index can be changed when necessary.
C
      IGIS=0
C
C Position to the user-selected portion of the outline dataset.
C
      CALL MAPIO (1)
      NSEG=0
C
C Read the next record (group of points).
C
  101 CALL MAPIO (2)
      NSEG=NSEG+1
C
C Check for the end of the desired data.
C
      IF (NPTS.EQ.0) GO TO 103
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
      IF (IWGF.EQ.0) THEN
        IF (SLAG.GT.BLAM.OR.BLAG.LT.SLAM) GO TO 101
        IF ((SLOG     .GT.BLOM.OR.BLOG     .LT.SLOM).AND.
     +      (SLOG-360..GT.BLOM.OR.BLOG-360..LT.SLOM).AND.
     +      (SLOG+360..GT.BLOM.OR.BLOG+360..LT.SLOM)) GO TO 101
      END IF
C
C See if the user wants to omit this point group.
C
      CALL MAPEOD (NOUT,NSEG,IDOS(NOUT)+IDLS,IDOS(NOUT)+IDRS,NPTS,PNTS)
      IF (NPTS.LE.1) GO TO 101
C
C If we've switched to a new group, set the color index, dotting, and
C dash pattern for the group.
C
      IF (IGID.NE.IGIS) THEN
        IF (IGIS.NE.0) CALL MAPCHI (-4-IGIS,0,0)
        CALL MAPCHI (4+IGID,IDOT,IOR(ISHIFT(32767,1),1))
        IGIS=IGID
      END IF
C
C Plot the group.
C
      CALL MAPIT (PNTS(1),PNTS(2),0)
C
      DO 102 K=2,NPTS-1
        CALL MAPIT (PNTS(2*K-1),PNTS(2*K),1)
  102 CONTINUE
C
      CALL MAPIT (PNTS(2*NPTS-1),PNTS(2*NPTS),2)
C
C Go get another group.
C
      GO TO 101
C
C Reset the color index, dotting, and dash pattern, if necessary.
C
  103 IF (IGIS.NE.0) CALL MAPCHI (-4-IGIS,0,0)
C
C If the limb lines have not already been drawn, do it now.
C
      IF (GRID.LE.0.) CALL MAPLMB
C
C Done.
C
      RETURN
C
      END
