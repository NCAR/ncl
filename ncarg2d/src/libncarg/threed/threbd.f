C
C	$Id: threbd.f,v 1.1.1.1 1992-04-17 22:31:48 ncargd Exp $
C
      BLOCKDATA THREBD
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
