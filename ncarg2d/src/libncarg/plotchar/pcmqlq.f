C
C $Id: pcmqlq.f,v 1.11 2008-07-27 00:17:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCMQLQ (XSUB,YSUB,CHRS,SIZE,ANGD,CNTR)
C
C When lower-quality characters have been selected by the user, this
C routine is called by PLCHHQ to write out the substrings, using one
C of the routines PLCHMQ or PLCHLQ.  The user may substitute his own
C version of this routine to call some other character-drawer.
C
C The arguments are identical to those of PLCHHQ.
C
      CHARACTER*(*) CHRS
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
C Call the appropriate routine.
C
      IF (IQUF.EQ.1) THEN
        CALL PLCHMQ (XSUB,YSUB,CHRS,SIZE,ANGD,CNTR)
        IF (ICFELL('PCMQLQ',1).NE.0) RETURN
      ELSE
        CALL PLCHLQ (XSUB,YSUB,CHRS,SIZE,ANGD,CNTR)
        IF (ICFELL('PCMQLQ',2).NE.0) RETURN
      END IF
C
C Done.
C
      RETURN
C
      END
