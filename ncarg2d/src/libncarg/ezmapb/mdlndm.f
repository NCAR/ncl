C
C $Id: mdlndm.f,v 1.6 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDLNDM (FLNM,ILVL,IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,
     +                                                           ULPR)
C
        CHARACTER*(*) FLNM
        INTEGER       ILVL,IAMA(*)
        REAL          XCRA(MCRA),YCRA(MCRA)
        INTEGER       MCRA,IAAI(MNOG),IAGI(MNOG),MNOG
C
        PARAMETER (MNAI=8000)
C
C This routine, given the name, FLNM, of a file of map data in the new
C (as of 4/98) format and the level, ILVL, at which those data are to
C be used, draws the map data described by the file, masked by the
C contents of the area map in IAMA, using the arrays XCRA and YCRA, each
C of which is dimensioned MCRA, as scratch arrays for X and Y coordinate
C data and the arrays IAAI and IAGI, each of which is dimensioned MNOG,
C as scratch arrays for area identifiers and group identifiers.  The
C routine ULPR is a user-supplied line-processing routine.
C
C The following COMMON blocks are used to share information with various
C other routines in EZMAPB or in EZMAP proper.
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCMX/  IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        INTEGER          IATY,ISCI,IPAR
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/  NAME(MNAI),FLNS
        CHARACTER*64     NAME
        CHARACTER*512    FLNS
        SAVE   /MAPCMY/
C
        COMMON /MAPCMZ/  NNMS,ILTY,IAIL,IAIR,BLAG,SLAG,BLOG,SLOG,
     +                   PNTS(200)
        INTEGER          NNMS,ILTY,IAIL,IAIR
        REAL             BLAG,SLAG,BLOG,SLOG,PNTS
        SAVE   /MAPCMZ/
C
C FLNL and FLND are character variables in which to form the names of
C files to be read (locally or in the NCAR Graphics database directory).
C
        CHARACTER*519    FLNL
        CHARACTER*1024   FLND
C
C CHRS is a buffer used to read name information.
C
        CHARACTER*1      CHRS(512)
C
C IIII is used in the process of mapping line types into color indices.
C
        INTEGER          IIII(4)
C
C SVOU is a character variable in which to save the value of the EZMAP
C internal parameter named 'OU'.
C
        CHARACTER*2      SVOU
C
C Declare other local variables.
C
        INTEGER          I,IAID,IFDE,ILTS,INTF,IOAL,IOAR,ISTA,IWGF,
     +                   LFND,LFNL,LFNM,LONM,MCHR,NCHR,NPTS
C
        DATA IIII / 5 , 7 , 6 , 8 /
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDLNDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Form the full names of the ".names" file (locally and in the NCAR
C Graphics database directory).
C
        LFNM=MDILNB(FLNM)
        IF (LFNM.GT.512) GO TO 901
C
        FLNL=FLNM(1:LFNM)//'.names'//CHAR(0)
        LFNL=LFNM+7
C
        CALL MPDBDI (FLND,ISTA)
        IF (ISTA.EQ.-1) GO TO 901
        DO 101 I=1,LEN(FLND)-7-LFNM
          IF (FLND(I:I).EQ.CHAR(0)) THEN
            FLND(I:LEN(FLND))='/'//FLNM(1:LFNM)//'.names'//CHAR(0)
            LFND=I+LFNM+7
            GO TO 102
          ENDIF
  101   CONTINUE
        GO TO 901
C
C If the name of the file to be used is not the same as the one last
C time, re-read name information.
C
  102   IF (FLNM.NE.FLNS) THEN
C
C Open the ".names" file.  Look for a local version first; if that one
C can't be found, look for one in the NCAR Graphics database directory.
C
          CALL NGOFRO (FLNL(1:LFNL),IFDE,ISTA)
          IF (ISTA.NE.0) THEN
            CALL NGOFRO (FLND(1:LFND),IFDE,ISTA)
            IF (ISTA.NE.0) GO TO 902
          END IF
C
C Clear the arrays into which the name information is to be read.
C
          DO 103 I=1,MNAI
            IATY(I)=0
            ISCI(I)=0
            IPAR(I)=0
            NAME(I)=' '
  103     CONTINUE
C
C Read and process all the name information.
C
          MCHR=0
          NCHR=0
C
  104     CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
          IF (MCHR.EQ.0) GO TO 105
          IF (IAID.LT.1.OR.IAID.GT.MNAI) GO TO 903
          CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,IATY(IAID))
          IF (MCHR.EQ.0) GO TO 904
          CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,ISCI(IAID))
          IF (MCHR.EQ.0) GO TO 904
          CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,IPAR(IAID))
          IF (MCHR.EQ.0) GO TO 904
          CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,LONM)
          IF (MCHR.EQ.0) GO TO 904
          NAME(IAID)=' '
          CALL MDRDCS (IFDE,CHRS,512,MCHR,NCHR,NAME(IAID)(1:LONM))
          IF (MCHR.EQ.0) GO TO 904
          GO TO 104
C
  105     CALL NGCLFI (IFDE)
C
C Save the file name, so that the read next time can be skipped.
C
          FLNS=FLNM
C
        END IF
C
C If EZMAP needs initialization, do it.
C
        CALL MDGETI ('IN',INTF)
C
        IF (INTF.NE.0) THEN
          CALL MDPINT
          IF (ICFELL('MDLNDM',2).NE.0) RETURN
        END IF
C
C Open the ".lines" file.  Again, look for a local version first and, if
C that one can't be found, look in the NCAR Graphics database directory.
C
        FLNL(LFNL-6:LFNL-1)='.lines'
        CALL NGOFRO (FLNL(1:LFNL),IFDE,ISTA)
        IF (ISTA.NE.0) THEN
          FLND(LFND-6:LFND-1)='.lines'
          CALL NGOFRO (FLND(1:LFND),IFDE,ISTA)
          IF (ISTA.NE.0) GO TO 905
        END IF
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window; otherwise, it is worth it and we do so.
C
        IWGF=0
        IF (BLAM-SLAM.GT.179.D0.AND.BLOM-SLOM.GT.359.D0) IWGF=1
C
C ILTS keeps track of changes in the line type, so that the color index
C can be changed when necessary.
C
        ILTS=0
C
C Read and process each of the lines.
C
  106   CALL NGRDIN (IFDE,NNMS,4,ISTA)
        IF (ISTA.NE.4) GO TO 906
C
        IF (NNMS.LE.1) GO TO 108
C
        IOAL=MDIOSA(IAIL,ILVL)
        IOAR=MDIOSA(IAIR,ILVL)
C
        CALL NGRDFL (IFDE,BLAG,4+NNMS,ISTA)
        IF (ISTA.NE.4+NNMS) GO TO 906
C
        IF (IWGF.EQ.0) THEN
          IF (SLAG.GT.BLAM.OR.BLAG.LT.SLAM) GO TO 106
          IF ((SLOG     .GT.BLOM.OR.BLOG     .LT.SLOM).AND.
     +        (SLOG-360..GT.BLOM.OR.BLOG-360..LT.SLOM).AND.
     +        (SLOG+360..GT.BLOM.OR.BLOG+360..LT.SLOM)) GO TO 106
        END IF
C
        IF (ILTY.LE.ILVL) THEN
          NPTS=NNMS/2
          CALL HLUMPCHLN (+2,ILTY,IOAL,IOAR,NPTS,PNTS)
          IF (NPTS.LE.1) GO TO 106
          IF (ILTY.NE.ILTS) THEN
            IF (ILTS.NE.0) THEN
              CALL MDPCHM (-IIII(MAX(2,MIN(5,ILTS))-1),0,
     +                          IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
              IF (ICFELL('MDLNDM',3).NE.0) RETURN
            END IF
            CALL MDPCHM (IIII(MAX(2,MIN(5,ILTY))-1),
     +                                           IOR(ISHIFT(32767,1),1),
     +                          IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
            IF (ICFELL('MDLNDM',4).NE.0) RETURN
            ILTS=ILTY
          END IF
          CALL MAPITM (PNTS(1),PNTS(2),0,
     +                          IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
          IF (ICFELL('MDLNDM',5).NE.0) GO TO 907
          DO 107 I=3,NNMS-3,2
            CALL MAPITM (PNTS(I),PNTS(I+1),1,
     +                          IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
            IF (ICFELL('MDLNDM',6).NE.0) GO TO 907
  107     CONTINUE
          CALL MAPITM (PNTS(NNMS-1),PNTS(NNMS),2,
     +                          IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
          IF (ICFELL('MDLNDM',7).NE.0) GO TO 907
          CALL MAPIQM (         IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
          IF (ICFELL('MDLNDM',8).NE.0) GO TO 907
          CALL HLUMPCHLN (-2,ILTY,IOAL,IOAR,NPTS,PNTS)
        END IF
C
        GO TO 106
C
  108   CALL NGCLFI (IFDE)
C
C Reset the color index, dotting, and dash pattern, if necessary.
C
        IF (ILTS.NE.0) THEN
          CALL MDPCHM (-IIII(MAX(2,MIN(5,ILTS))-1),0,
     +                          IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
          IF (ICFELL('MDLNDM',9).NE.0) RETURN
        END IF
C
C Use the EZMAPA routine MAPBLM to draw a perimeter and limb lines (if
C any).
C
        CALL MDGETC ('OU',SVOU)
        CALL MDSETC ('OU','NO')
        CALL MDPBLM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,MNOG,ULPR)
        IF (ICFELL('MDLNDM',10).NE.0) RETURN
        CALL MDSETC ('OU',SVOU)
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL SETER ('MDLNDM - Can''t form name of ".names" file',10,1)
        RETURN
C
  902   CALL SETER ('MDLNDM - Can''t open the ".names" file',11,1)
        RETURN
C
  903   CALL SETER ('MDLNDM - Read bad index from ".names" file',12,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  904   CALL SETER ('MDLNDM - Read error on ".names" file',13,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  905   CALL SETER ('MDLNDM - Can''t open the ".lines" file',14,1)
        RETURN
C
  906   CALL SETER ('MDLNDM - Read error on ".lines" file',15,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  907   CALL NGCLFI (IFDE)
        RETURN
C
      END
