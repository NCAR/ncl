C
C $Id: mplnri.f,v 1.2 1998-04-30 22:43:57 kennison Exp $
C
      SUBROUTINE MPLNRI (FLNM)
C
        PARAMETER (MNAI=2000)
C
        CHARACTER*(*) FLNM
C
C This routine, given the name, FLNM, of a file of map data in the new
C (as of 4/98) format, reads the ".names" file and stores information
C from it in common blocks MAPCMX and MAPCMY.
C
C The following COMMON blocks are used to share information with various
C other routines in EZMAPB.
C
        COMMON /MAPCMX/ IATY(MNAI),ISCI(MNAI),IPAR(MNAI)
        SAVE   /MAPCMX/
C
        COMMON /MAPCMY/ NAME(MNAI),FLNS
        CHARACTER*64    NAME,FLNS
        SAVE   /MAPCMY/
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
C Check for an uncleared prior error.
C
        IF (ICFELL('MPLNRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
C Open the ".names" file.  Look for a local version first; if that one
C can't be found, look for one in the NCAR Graphics database directory.
C
  102   CALL NGOFRO (FLNL(1:LFNL),IFDE,ISTA)
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
  103   CONTINUE
C
C Read and process all the name information.
C
        MCHR=0
        NCHR=0
C
  104   CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
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
  105   CALL NGCLFI (IFDE)
C
C Save the file name, so that other routines that read the info will
C know it's unnecessary.
C
        FLNS=FLNM
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL SETER ('MPLNRI - Can''t form name of ".names" file',2,1)
        RETURN
C
  902   CALL SETER ('MPLNRI - Can''t open the ".names" file',3,1)
        RETURN
C
  903   CALL SETER ('MPLNRI - Read bad index from ".names" file',4,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  904   CALL SETER ('MPLNRI - Read error on ".names" file',5,1)
        CALL NGCLFI (IFDE)
        RETURN
C
      END
