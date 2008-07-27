C
C $Id: ftitle.f,v 1.6 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FTITLE (MTST)
C
C The routine FTITLE reads from a "card input unit" groups of cards,
C each of which describes a single title frame that is to be displayed
C for a specified number of seconds using characters of a specified
C size.  FTITLE does the call to STITLE which is required to create the
C desired frames.
C
C FTITLE limits the number of lines on a given frame to "MCDS" (by
C default, 120).  If more lines than this are desired, reset the value
C in the following PARAMETER statement:
C
        PARAMETER (MCDS=120)
        CHARACTER*100 CRDS(MCDS)
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('FTITLE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set TGPN, which is the size of the gap preceding the next title (TGP1
C preceding the first title), and TGPF, which is the size of the final
C gap (zero so that, if no titles are found, we get no gap at all).
C
        TGPN=TGP1
        TGPF=0.
C
C Read up the first card of the next batch (done if end of file or zero
C card count are seen, error if there are too many title lines).
C
  101   NCDS=0
C
        READ (INCU,'(I5,2F5.1)',END=103,ERR=901) NCDS,TIME,SIZE
C
        IF (NCDS.LE.0) GO TO 103
C
        IF (NCDS.GT.MCDS) GO TO 902
C
C Compute the vertical size of a line and of the gap between lines and
C the sum of those.
C
        ISIZ=INT(SIZE*PCSZ)
        IGAP=INT(SIZE*GPSZ)
C
        ISUM=ISIZ+IGAP
C
C Set the X/Y coordinates of the first line.
C
        IXPS=64+448*ICOP
        IYPS=512+((NCDS-1)*ISUM)/2
C
C Set up the CRDS array for input to STITLE.
C
        DO 102 I=1,NCDS
          WRITE (CRDS(I)(1:20),'(3I5,F5.1)') IXPS,IYPS,ICOP,SIZE
          READ (INCU,'(A80)',ERR=902,END=903) CRDS(I)(21:100)
          IYPS=IYPS-ISUM
  102   CONTINUE
C
C Output a gap before calling STITLE.
C
        IF (TGPN.GT.0.) THEN
          CALL SLOGAP (TGPN,MTST)
          IF (ICFELL('FTITLE',2).NE.0) RETURN
        END IF
C
C Call STITLE to actually produce the desired frames.
C
        CALL STITLE (CRDS,NCDS,512,512,0.,TIME,0.,MTST)
        IF (ICFELL('FTITLE',3).NE.0) RETURN
C
C Reset TGPN and TGPF to the appropriate values for the gaps preceding
C the next label and following the last label.
C
        TGPN=TGP2
        TGPF=TGP2+TGP3
C
C Go back to read the next batch of cards.
C
        GO TO 101
C
C End of input detected.  Output a final gap.
C
  103   IF (TGPF.GT.0.) THEN
          CALL SLOGAP (TGPF,MTST)
          IF (ICFELL('FTITLE',4).NE.0) RETURN
        END IF
C
C Normal return.
C
        RETURN
C
C Error exits.
C
  901   CALL SETER ('FTITLE - READ ERROR ON CARD INPUT UNIT',5,1)
        RETURN
C
  902   CALL SETER ('FTITLE - TOO MANY INPUT CARDS IN GROUP',6,1)
        RETURN
C
  903   CALL SETER ('FTITLE - PREMATURE EOF ON CARD INPUT UNIT',7,1)
        RETURN
C
      END
