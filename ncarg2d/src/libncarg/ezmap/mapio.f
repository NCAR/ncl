C
C $Id: mapio.f,v 1.6 1994-03-16 23:51:43 kennison Exp $
C
      SUBROUTINE MAPIO (IACT)
C
C This routine performs all positioning and input of the outline dataset
C for MAPLOT.  The argument IACT specifies what is to be done:  1 asks
C that the dataset be positioned at the beginning of the desired "file",
C 2 that the next record be read.
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE /MAPCM3/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
      IF (IACT.EQ.1) THEN
C
C Open the dataset using this code for a typical Unix system ...
C
        CALL OPENMP (ITPN)
C
C or this code for an old COS system ...
C
C       ITPN=6LEZMPCD
C       IF (IFDNT(ITPN).EQ.0) THEN
C         CALL SDACCESS (IERR,ITPN)
C         IF (IERR.NE.0) GO TO 901
C       END IF
C
C rewind it, ...
C
        REWIND ITPN
C
C and position to the desired file.
C
        IF (NOUT.NE.1) THEN
          ITMP=NOUT
  101     READ (ITPN,END=902,ERR=903) NPTS,IGID,IDLS,IDRS,
     +                                BLAG,SLAG,BLOG,SLOG,
     +                                (PNTS(I),I=1,NPTS)
          IF (NPTS.GT.1) GO TO 101
          ITMP=ITMP-1
          IF (ITMP.GT.1) GO TO 101
        END IF
C
      ELSE
C
C Read the next record.
C
        READ (ITPN,ERR=903) NPTS,IGID,IDLS,IDRS,
     +                      BLAG,SLAG,BLOG,SLOG,
     +                      (PNTS(I),I=1,NPTS)
        NPTS=NPTS/2
C
      END IF
C
C Done.
C
      RETURN
C
C Error exits.
C
C 901 IIER=17
C     CALL SETER ('MAPIO - OUTLINE DATASET IS UNREADABLE',IIER,1)
C     NOUT=0
C     RETURN
C
  902 IIER=18
      CALL SETER ('MAPIO - EOF ENCOUNTERED IN OUTLINE DATASET',IIER,1)
      NOUT=0
      RETURN
C
  903 IIER=23
      CALL SETER ('MAPIO - ERROR ON READ OF OUTLINE DATASET',IIER,1)
      NOUT=0
      RETURN
C
      END
