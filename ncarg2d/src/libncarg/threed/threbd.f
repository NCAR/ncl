C
C $Id: threbd.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE THREBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA THREBDX
      COMMON /TEMPRT/ RZERO
      COMMON /SET31/  ISCALE     ,XMIN       ,XMAX       ,YMIN       ,
     1                YMAX       ,BIGD       ,R0         ,NLX        ,
     2                NBY        ,NRX        ,NTY
      COMMON /TCK31/  TMAGU      ,TMINU      ,TMAGV      ,TMINV      ,
     1                TMAGW      ,TMINW
      COMMON /THRINT/ ITHRMJ     ,ITHRMN     ,ITHRTX
      DATA RZERO/0./
      DATA NLX,NBY,NRX,NTY/10,10,1010,1010/
      DATA TMAGU,TMINU,TMAGV,TMINV,TMAGW,TMINW/12.,8.,12.,8.,12.,8./
      DATA ITHRMJ,ITHRMN,ITHRTX/ 1,1,1/
      END
