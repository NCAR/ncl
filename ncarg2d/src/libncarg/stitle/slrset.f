C
C $Id: slrset.f,v 1.2 2000-07-12 16:25:59 haley Exp $
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
      SUBROUTINE SLRSET
C
C This routine, when called, restores the default values of the
C internal parameters of STITLE.
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C The common block SLCLRS holds color-table parameters.
C
        COMMON /SLCLRS/ CRED(256),CGRN(256),CBLU(256),LOCI(256),NOCI
        SAVE   /SLCLRS/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('SLRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Do it.
C
        GPSZ=40.
        IBGC=0
        IBGF=-2
        ICOP=1
        IDOT=0
        IFGC=1
        IFGF=-2
        IJMP=300
        IMAP=100
        INCU=5
        IWLU=4
        IWRK=-1
        IWWI=9
        NOCI=0
        IXND=512
        IXST=512
        OORV=1.E12
        PCSZ=21.
        RNFS=24.
        RVPB=0.
        RVPL=0.
        RVPR=1.
        RVPT=1.
        TFIN=0.
        TFOU=0.
        TGP1=1.
        TGP2=.5
        TGP3=0.
C
C Done.
C
        RETURN
C
      END
