C
C $Id: mdlnri.f,v 1.4 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDLNRI (FLNM)
C
        CHARACTER*(*) FLNM
C
        PARAMETER (MNAI=8000)
C
C This routine, given the name, FLNM, of a file of map data in the new
C (as of 4/98) format, reads the ".names" file and stores information
C from it in common blocks MAPCMX and MAPCMY.
C
C The following COMMON blocks are used to share information with various
C other routines in EZMAPB.
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
C Declare other local variables.
C
        INTEGER          I,IAID,IFDE,ISTA,LFND,LFNL,LFNM,LONM,MCHR,NCHR
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDLNRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
  104   CALL MDRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
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
  901   CALL SETER ('MDLNRI - Can''t form name of ".names" file',2,1)
        RETURN
C
  902   CALL SETER ('MDLNRI - Can''t open the ".names" file',3,1)
        RETURN
C
  903   CALL SETER ('MDLNRI - Read bad index from ".names" file',4,1)
        CALL NGCLFI (IFDE)
        RETURN
C
  904   CALL SETER ('MDLNRI - Read error on ".names" file',5,1)
        CALL NGCLFI (IFDE)
        RETURN
C
      END
