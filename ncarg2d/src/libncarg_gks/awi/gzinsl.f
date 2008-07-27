C
C	$Id: gzinsl.f,v 1.8 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
