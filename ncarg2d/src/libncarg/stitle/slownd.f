C
C $Id: slownd.f,v 1.3 2000-07-12 16:25:59 haley Exp $
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
      SUBROUTINE SLOWND
C
C Draw the outline of the windowed area.
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
        CALL PLOTIF (  0.,  0.,2)
        IF (ICFELL('SLOWND',1).NE.0) RETURN
        CALL PLOTIF (RVPL,RVPB,0)
        IF (ICFELL('SLOWND',2).NE.0) RETURN
        CALL PLOTIF (RVPR,RVPB,1)
        IF (ICFELL('SLOWND',3).NE.0) RETURN
        CALL PLOTIF (RVPR,RVPT,1)
        IF (ICFELL('SLOWND',4).NE.0) RETURN
        CALL PLOTIF (RVPL,RVPT,1)
        IF (ICFELL('SLOWND',5).NE.0) RETURN
        CALL PLOTIF (RVPL,RVPB,1)
        IF (ICFELL('SLOWND',6).NE.0) RETURN
        CALL PLOTIF (  0.,  0.,2)
        IF (ICFELL('SLOWND',7).NE.0) RETURN
        RETURN
      END
