C
C $Id: pcmqlq.f,v 1.9 2000-07-12 16:24:59 haley Exp $
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
