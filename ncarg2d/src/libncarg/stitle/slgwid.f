C
C $Id: slgwid.f,v 1.4 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLGWID
C
C Get the ID of the first active workstation.
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
C Get the required ID from GKS.
C
        CALL GQACWK (1,IERR,NOAW,IWRK)
C
        IF (IERR.NE.0) THEN
          CMSG(1:34)='SLGWID - ERROR RETURN FROM GQACWK:'
          WRITE (CMSG(35:42),'(I8)') IERR
          CALL SETER (CMSG(1:42),1,1)
          RETURN
        END IF
C
        IF (NOAW.LT.1) THEN
          CALL SETER ('SLGWID - NO ACTIVE WORKSTATIONS',2,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
