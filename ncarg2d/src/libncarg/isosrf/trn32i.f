C
C $Id: trn32i.f,v 1.4 2000-08-22 15:04:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
C The subroutine TRN32I.
C --- ---------- -------
C
      SUBROUTINE TRN32I (UT,VT,WT,XT,YT,ZT,IENT)
C
C This routine provides a temporary interface for ISTR32 until such
C time as all calls to TRN32I can be found and modified (which may
C be never, as there are likely to be some such calls in user code).
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Pass the arguments on to ISTR32.
C
        CALL ISTR32 (UT,VT,WT,XT,YT,ZT,IENT)
C
C If necessary, convert the returned coordinates from the fractional
C system to the metacode system.
C
        IF (IENT.NE.1.AND.ISCALE.NE.0) THEN
          XT=32767.*XT
          YT=32767.*YT
        END IF
C
C Done.
C
        RETURN
C
      END
