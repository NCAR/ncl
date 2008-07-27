C
C $Id: g01io.h,v 1.5 2008-07-27 03:55:37 haley Exp $
C
C The use of this Software is governed by a License Agreement.
C

      COMMON  /G01IO/   MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY(256)     ,
     +                  MOBFSZ  ,MOUTBF(720)    ,MBFPOS ,MFGLUN ,
     +                  MXBITS  ,MDTYPE ,MNFFLG ,MBMFLG ,MEMFLG
      INTEGER           MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY  ,MOBFSZ ,
     +                  MBFPOS  ,MFGLUN ,MOUTBF ,MXBITS ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
      COMMON  /G01CHA/  MPNAME
      CHARACTER*80      MPNAME
