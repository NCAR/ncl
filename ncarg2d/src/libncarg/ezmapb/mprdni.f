C
C $Id: mprdni.f,v 1.1 1998-04-16 20:45:53 kennison Exp $
C
      SUBROUTINE MPRDNI (FLNM)
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
        CHARACTER*64    NAME
        CHARACTER*128   FLNS
        SAVE   /MAPCMY/
C
C FLNT is a character variable in which to form the name of a file to
C be read.
C
        CHARACTER*128 FLNT
C
C CHRS is a buffer used to read name information.
C
        CHARACTER*1 CHRS(512)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MPRDNI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Form the full name of the ".names" file.
C
        LFNM=MPILNB(FLNM)
        CALL MPDBDI (FLNT,ISTA)
        IF (ISTA.EQ.-1) GO TO 901
        DO 101 I=1,121-LFNM
          IF (FLNT(I:I).EQ.CHAR(0)) THEN
            FLNT(I:128)='/'//FLNM(1:LFNM)//'.names'//CHAR(0)
            LFLT=I+LFNM+7
            GO TO 102
          ENDIF
  101   CONTINUE
        GO TO 901
C
C Open the ".names" file.
C
  102   CALL NGOFRO (FLNT(1:LFLT),IFDE,ISTA)
        IF (ISTA.NE.0) GO TO 902
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
        FLNS=FLNT
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL SETER ('MPRDNI - Can''t form name of ".names" file',2,1)
        RETURN
C
  902   CALL SETER ('MPRDNI - Can''t open the ".names" file',3,1)
        RETURN
C
  903   CALL SETER ('MPRDNI - Read bad index from ".names" file',4,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  904   CALL SETER ('MPRDNI - Read error on ".names" file',5,1)
        CALL NGCLFI (IFDE)
        RETURN
C
      END
