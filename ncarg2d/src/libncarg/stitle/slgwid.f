C
C $Id: slgwid.f,v 1.2 2000-07-12 16:25:59 haley Exp $
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
