C
C $Id: pcrset.f,v 1.4 2008-07-27 00:17:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCRSET
C
C Calling PCRSET resets all internal parameters of PLOTCHAR to their
C default values.
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),IBNU,
     +                IBXC(3),IBXF,ICEN,IORD,IOUC,IOUF,IPCC,IQUF,
     +                ISHC,ISHF,ITEF,JCOD,LSCI(16),NFCC,NODF,RBXL,
     +                RBXM,RBXX,RBXY,ROLW,RPLW,RSLW,SHDX,SHDY,SIZA,
     +                SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,XEND,
     +                XMUL(3),YBEG,YCEN,YEND,YMUL(3),ZINX,ZINY,ZINZ
      SAVE   /PCPRMS/
C
      COMMON /PCSVEM/ ICOD,IDDA(8625),IDDL,RDGU(7000),IDPC(256),IERU,
     +                INDA(789),INDL,INIT,IVCO,IVDU,NBPW,NPPW
      SAVE   /PCSVEM/
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCRSET - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Reset the values of internal parameters in common blocks.
C
      ADDS=0.
      CONS=0.
      DSTB=0.
      DSTL=0.
      DSTR=0.
      DSTT=0.
      HPIC(1)=21.
      HPIC(2)=13.
      HPIC(3)= 9.
      IBNU=3
      IBXC(1)=-1
      IBXC(2)=-1
      IBXC(3)=-1
      IBXF=0
      ICEN=0
      IMAP=0
      IORD=1
      IOUC=1
      IOUF=0
      IPCC=-1
      IQUF=0
      ISHC=0
      ISHF=0
      ITEF=0
      JCOD=0
      DO 101 I=1,16
        LSCI(I)=-1
  101 CONTINUE
      NFCC=ICHAR(':')
      NODF=0
      OORV=0.
      RBXL=0.
      RBXM=.15
      RBXX=-.05
      RBXY=-.05
      RHTW=1.75
      ROLW=0.
      RPLW=0.
      RSLW=0.
      SHDX=-.05
      SHDY=-.05
      SIZA=.888888888888888
      SSIC=7.
      SSPR=10.
      SUBS=0.
      VPIC(1)=32.
      VPIC(2)=20.
      VPIC(3)=14.
      WPIC(1)=16.
      WPIC(2)=12.
      WPIC(3)= 8.
      XBEG=0.
      XCEN=0.
      XEND=0.
      XMUL(1)=1.
      XMUL(2)=1.
      XMUL(3)=1.
      YBEG=0.
      YCEN=0.
      YEND=0.
      YMUL(1)=1.
      YMUL(2)=1.
      YMUL(3)=1.
      ZINX=1.
      ZINY=1.
      ZINZ=1.
C
C Done.
C
      RETURN
C
      END
