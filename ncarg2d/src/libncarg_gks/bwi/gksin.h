C
C $Id: gksin.h,v 1.5 2008-07-27 03:55:38 haley Exp $
C
C The use of this Software is governed by a License Agreement.
C

      COMMON/GKSIN1/ FCODE , CONT  ,
     +               IL1   , IL2   , ID(128)       ,
     +               IC1   , IC2   , IC(128)       ,
     +               RL1   , RL2   , RX(128)       , RY(128)       ,
     +               STRL1 , STRL2 , RERR
      COMMON/GKSIN2/ STR
      INTEGER        FCODE , CONT  , IL1   , IL2   , ID    , IC1   ,
     +               IC2   , IC    , RL1   , RL2   , STRL1 , STRL2 ,
     +               RERR
      CHARACTER*160  STR
      REAL           RX    , RY
