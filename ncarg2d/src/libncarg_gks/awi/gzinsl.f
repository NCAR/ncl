C
C	$Id: gzinsl.f,v 1.6 2000-07-12 16:40:05 haley Exp $
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
      SUBROUTINE GZINSL
C
C  This subroutine initializes the GKS state list.
C
      include 'gkscom.h'
C
      DO 10 I=1,MOPWK
        SOPWK(I) = -1
        LXWKID(I) = -1
   10 CONTINUE
      DO 20 I=1,MACWK
        SACWK(I) = -1
   20 CONTINUE
      DO 30 I=1,MACWK
        SWKTP(I) = -1
   30 CONTINUE
      CTXFP (1)  = 1
      CTXFP (2)  = 0
      CCHUP (1)  = 0.
      CCHUP (2)  = 1.
      CTXAL (1)  = 0
      CTXAL (2)  = 0
      CPA   (1)  = 1.
      CPA   (2)  = 1.
      CPARF (1)  = 0.
      CPARF (2)  = 0.
      LSNT  (1)  = 0
      LSNT  (2)  = 1
      NTWN(1,1)  = 0.
      NTWN(1,2)  = 1.
      NTWN(1,3)  = 0.
      NTWN(1,4)  = 1.
      NTWN(2,1)  = 0.
      NTWN(2,2)  = 1.
      NTWN(2,3)  = 0.
      NTWN(2,4)  = 1.
      NTVP(1,1)  = 0.
      NTVP(1,2)  = 1.
      NTVP(1,3)  = 0.
      NTVP(1,4)  = 1.
      NTVP(2,1)  = 0.
      NTVP(2,2)  = 1.
      NTVP(2,3)  = 0.
      NTVP(2,4)  = 1.
      CPLI       = 1
      CLN        = 1
      CLWSC      = 1.
      CPLCI      = 1
      CLNA       = GINDIV
      CLWSCA     = GINDIV
      CPLCIA     = GINDIV
      CPMI       = 1
      CMK        = 3
      CMKS       = 1.
      CPMCI      = 1
      CMKA       = GINDIV
      CMKSA      = GINDIV
      CPMCIA     = GINDIV
      CTXI       = 1
      CCHXP      = 1.0
      CCHSP      = 0.
      CTXCI      = 1
      CTXFPA     = GINDIV
      CCHXPA     = GINDIV
      CCHSPA     = GINDIV
      CTXCIA     = GINDIV
      CCHH       = .01
      CTXP       = 0
      CFAI       = 1
      CFAIS      = 1
      CFASI      = 1
      CFACI      = 1
      CFAISA     = GINDIV
      CFASIA     = GINDIV
      CFACIA     = GINDIV
      CNT        = 0
      CCLIP      = 1
C
      RETURN
      END
