C
C $Id: mplnam.f,v 1.6 1998-05-24 00:40:53 kennison Exp $
C
      SUBROUTINE MPLNAM (FLNM,ILVL,IAMA)
C
        PARAMETER (MNAI=2000)
C
        CHARACTER*(*) FLNM
        DIMENSION IAMA(*)
C
C This routine, given the name, FLNM, of a file of map data in the new
C (as of 4/98) format and the level, ILVL, at which those data are to
C be used, sends the line data from the file to the area map array IAMA.
C
C The following COMMON blocks are used to share information with various
C other routines in EZMAPB or in EZMAP proper.
C
        COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,
     +                  URNG,VRNG,BLAM,SLAM,BLOM,SLOM,ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/ NAME(MNAI),FLNS
        CHARACTER*64    NAME,FLNS
        SAVE   /MAPCMY/
C
        COMMON /MAPCMZ/ NNMS,ILTY,IAIL,IAIR,BLAG,SLAG,BLOG,SLOG,
     +                  PNTS(200)
        SAVE   /MAPCMZ/
C
C FLNL and FLND are character variables in which to form the names of
C files to be read (locally or in the NCAR Graphics database directory).
C
        CHARACTER*71  FLNL
        CHARACTER*128 FLND
C
C CHRS is a buffer used to read name information.
C
        CHARACTER*1 CHRS(512)
C
C XCRA and YCRA are used to hold coordinates defining a line segment
C conveying coloring information to the area map in the degenerate
C case where no lines from the map dataset do so.
C
        DIMENSION XCRA(2),YCRA(2)
C
C SVOU is a character variable in which to save the value of the EZMAP
C internal parameter named 'OU'.
C
        CHARACTER*2 SVOU
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MPLNAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Form the full names of the ".names" file (locally and in the NCAR
C Graphics database directory).
C
        LFNM=MPILNB(FLNM)
C
        FLNL=FLNM(1:LFNM)//'.names'//CHAR(0)
        LFNL=LFNM+7
C
        CALL MPDBDI (FLND,ISTA)
        IF (ISTA.EQ.-1) GO TO 901
        DO 101 I=1,121-LFNM
          IF (FLND(I:I).EQ.CHAR(0)) THEN
            FLND(I:128)='/'//FLNM(1:LFNM)//'.names'//CHAR(0)
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
  104     CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
          IF (MCHR.EQ.0) GO TO 105
          IF (IAID.LT.1.OR.IAID.GT.MNAI) GO TO 903
          CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,IATY(IAID))
          IF (MCHR.EQ.0) GO TO 904
          CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,ISCI(IAID))
          IF (MCHR.EQ.0) GO TO 904
          CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,IPAR(IAID))
          IF (MCHR.EQ.0) GO TO 904
          CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,LONM)
          IF (MCHR.EQ.0) GO TO 904
          NAME(IAID)=' '
          CALL MPRDCS (IFDE,CHRS,512,MCHR,NCHR,NAME(IAID)(1:LONM))
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
C If EZMAP needs initialization, do nothing further.
C
        CALL MPGETI ('IN',INTF)
        IF (INTF.NE.0) RETURN
C
C Use the EZMAPA routine MAPBLA to generate limb lines (if any)
C and a perimeter and send them to the area map.
C
        CALL MAPGTC ('OU',SVOU)
        CALL MAPSTC ('OU','NO')
        CALL MAPBLA (IAMA)
        IF (ICFELL('MPLNAM',2).NE.0) RETURN
        CALL MAPSTC ('OU',SVOU)
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
C Get the group identifier to be used for geographical entities.
C
        CALL MPGETI ('G1',IGI1)
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window; otherwise, it is worth it and we do so.
C
        IWGF=0
        IF (BLAM-SLAM.GT.179.9999.AND.BLOM-SLOM.GT.359.9999) IWGF=1
C
C Save some pointers that will tell us whether anything actually got
C put into the area map, so that, if not, we can take remedial action.
C
        IAM5=IAMA(5)
C
C Read and process each of the lines.
C
  106   CALL NGRDIN (IFDE,NNMS,4,ISTA)
        IF (ISTA.NE.4) GO TO 906
C
        IF (NNMS.LE.1) GO TO 108
C
        IOAL=MPIOSA(IAIL,ILVL)
        IOAR=MPIOSA(IAIR,ILVL)
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
        IF (IOAL.NE.IOAR) THEN
          NPTS=NNMS/2
          CALL HLUMPCHLN (+1,ILTY,IOAL,IOAR,NPTS,PNTS)
          IF (NPTS.LE.1) GO TO 106
          CALL MAPITA (PNTS(1),PNTS(2),0,IAMA,IGI1,IOAL,IOAR)
          IF (ICFELL('MPLNAM',3).NE.0) GO TO 907
          DO 107 I=3,NNMS-3,2
            CALL MAPITA (PNTS(I),PNTS(I+1),1,IAMA,IGI1,IOAL,IOAR)
            IF (ICFELL('MPLNAM',4).NE.0) GO TO 907
  107     CONTINUE
          CALL MAPITA (PNTS(NNMS-1),PNTS(NNMS),2,IAMA,IGI1,IOAL,IOAR)
          IF (ICFELL('MPLNAM',5).NE.0) GO TO 907
          CALL MAPIQA (IAMA,IGI1,IOAL,IOAR)
          IF (ICFELL('MPLNAM',6).NE.0) GO TO 907
          CALL HLUMPCHLN (-1,ILTY,IOAL,IOAR,NPTS,PNTS)
        END IF
C
        GO TO 106
C
  108   CALL NGCLFI (IFDE)
C
C See if anything was actually put into the area map and, if not, take
C action to supply AREAS with a correct area identifier.  Again, look
C for a local version of the required file and, if it can't be found,
C look in the NCAR Graphics database directory.
C
        IF (IAMA(5).EQ.IAM5) THEN
          FLNL(LFNL-6:LFNL-1)='.areas'
          CALL NGOFRO (FLNL(1:LFNL),IFDE,ISTA)
          IF (ISTA.NE.0) THEN
            FLND(LFND-6:LFND-1)='.areas'
            CALL NGOFRO (FLND(1:LFND),IFDE,ISTA)
            IF (ISTA.NE.0) GO TO 113
          END IF
          NTMS=0
          MCHR=0
          NCHR=0
          DO 111 IDIV=0,15
            NROW=2**IDIV
            NCOL=2*NROW
            DO 110 J=1,NROW
              RLAT=-90.+(REAL(J)-.5)*(180./REAL(NROW))
              DO 109 I=1,NCOL
                RLON=-180.+(REAL(I)-.5)*(360./REAL(NCOL))
                IF (NTMS.EQ.0) THEN
                  CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,NTMS)
                  IF (MCHR.EQ.0) GO TO 112
                  CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
                  IF (MCHR.EQ.0) GO TO 112
                END IF
                CALL MAPTRA (RLAT,RLON,UVAL,VVAL)
                IF (UVAL.NE.1.E12) THEN
                  XCRA(1)=UMIN
                  XCRA(2)=UMAX
                  YCRA(1)=VMIN
                  YCRA(2)=VMAX
                  IAID=MPIOSA (IAID,ILVL)
                  CALL AREDAM (IAMA,XCRA,YCRA,2,IGI1,IAID,IAID)
                  IF (ICFELL('MPLNAM',7).NE.0) GO TO 907
                  GO TO 112
                END IF
                NTMS=NTMS-1
  109         CONTINUE
  110       CONTINUE
  111     CONTINUE
  112     CALL NGCLFI (IFDE)
C
        END IF
C
C Done.
C
  113   RETURN
C
C Error exits.
C
  901   CALL SETER ('MPLNAM - Can''t form name of ".names" file',8,1)
        RETURN
C
  902   CALL SETER ('MPLNAM - Can''t open the ".names" file',9,1)
        RETURN
C
  903   CALL SETER ('MPLNAM - Read bad index from ".names" file',10,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  904   CALL SETER ('MPLNAM - Read error on ".names" file',11,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  905   CALL SETER ('MPLNAM - Can''t open the ".lines" file',12,1)
        RETURN
C
  906   CALL SETER ('MPLNAM - Read error on ".lines" file',13,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  907   CALL NGCLFI (IFDE)
        RETURN
C
      END
