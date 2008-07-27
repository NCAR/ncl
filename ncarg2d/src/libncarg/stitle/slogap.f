C
C $Id: slogap.f,v 1.5 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLOGAP (TIME,MTST)
C
C Provide a TIME-second gap in the movie (or, if MTST is non-zero, a
C test frame representing such a gap).
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C Define a character variable in which messages may be formed.
C
        CHARACTER*42 CMSG
C
C Check for an uncleared prior error.
C
        IF (ICFELL('SLOGAP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C NFRM is the number of blank frames required for a TIME-second gap.
C
        NFRM=INT(RNFS*TIME+.5)
C
C There's nothing to do if a non-positive number of frames is specified.
C
        IF (NFRM.LE.0) RETURN
C
C Set the background and the default foreground colors as defined by
C STITLE's color table.  (This really has an effect only when FTITLE
C puts out an initial gap: it ensures that the gap will be similar
C to the gaps that are generated after calling STITLE.)
C
        CALL SLGCLR (IBGC,BRED,BGRN,BBLU)
        IF (ICFELL('SLOGAP',2).NE.0) RETURN
        CALL GSCR   (IWRK,IBGC,BRED,BGRN,BBLU)
C
        CALL SLGCLR (IFGC,FRED,FGRN,FBLU)
        IF (ICFELL('SLOGAP',3).NE.0) RETURN
        CALL GSCR   (IWRK,IFGC,FRED,FGRN,FBLU)
C
C Output depends on whether this is a real run or a practice run.
C
        IF (MTST.EQ.0) THEN
C
C Real run - output NFRM blank frames.
C
          DO 101 IFRM=1,NFRM
            CALL SLFRME
            IF (ICFELL('SLOGAP',4).NE.0) RETURN
  101     CONTINUE
C
        ELSE
C
C Practice run - output a single frame saying how many blank frames it
C stands for, making sure to use the default foreground color.  Again,
C the code to ensure the latter is necessary only during a call by
C FTITLE to put out an initial gap.
C
          CALL GQTXCI (IERR,ISTX)
          IF (IERR.NE.0) THEN
            CMSG(1:34)='SLOGAP - ERROR RETURN FROM GQTXCI:'
            WRITE (CMSG(35:42),'(I8)') IERR
            CALL SETER (CMSG(1:42),5,1)
            RETURN
          END IF
          CALL GSTXCI (IFGC)

          WRITE (CMSG,'(I3)') NFRM
          CMSG(4:16)=' BLANK FRAMES'
          CALL WTSTR (.5,.5,CMSG(1:16),2,0,0)
          IF (ICFELL('SLOGAP',6).NE.0) RETURN
          CALL SLFRME
          IF (ICFELL('SLOGAP',7).NE.0) RETURN
C
          CALL GSTXCI (ISTX)
C
        END IF
C
C Done.
C
        RETURN
C
      END
