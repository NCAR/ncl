C
C $Id: slbkgd.f,v 1.3 2000-07-12 16:25:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
