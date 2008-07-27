C
C $Id: slrset.f,v 1.4 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
