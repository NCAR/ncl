C
C $Id: slbkgd.f,v 1.5 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLBKGD
C
C This routine fills the STITLE viewport with the background color
C defined by the color index IBGC (the internal parameter 'BGC').
C It is called by STITLE only when the value of IBGC is other than
C zero.  (The assumption is that, when IBGC is zero, GKS handles
C background fill.)
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C Declare arrays in which to define a rectangle to be filled.
C
        DIMENSION XCRA(4),YCRA(4)
C
C Define a character variable in which messages may be formed.
C
        CHARACTER*42 CMSG
C
C Define the rectangle to be filled.
C
        XCRA(1)=RVPL
        XCRA(2)=RVPR
        XCRA(3)=RVPR
        XCRA(4)=RVPL
        YCRA(1)=RVPB
        YCRA(2)=RVPB
        YCRA(3)=RVPT
        YCRA(4)=RVPT
C
C Save the current "fill area interior style" and reset to "solid".
C
        CALL GQFAIS (IERR,ISFS)
        IF (IERR.NE.0) THEN
          CMSG(1:34)='SLBKGD - ERROR RETURN FROM GQFAIS:'
          WRITE (CMSG(35:42),'(I8)') IERR
          CALL SETER (CMSG(1:42),1,1)
          RETURN
        END IF
        CALL GSFAIS (1)
C
C Save the current "fill area color index" and reset it to the index of
C the background color.
C
        CALL GQFACI (IERR,ISFC)
        IF (IERR.NE.0) THEN
          CMSG(1:34)='SLBKGD - ERROR RETURN FROM GQFACI:'
          WRITE (CMSG(35:42),'(I8)') IERR
          CALL SETER (CMSG(1:42),2,1)
          RETURN
        END IF
        CALL GSFACI (IBGC)
C
C Fill the rectangle.
C
        CALL GFA (4,XCRA,YCRA)
C
C Restore the original "fill area interior style" and "fill area color
C index".
C
        CALL GSFAIS (ISFS)
        CALL GSFACI (ISFC)
C
C Done.
C
        RETURN
C
      END
