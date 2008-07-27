C
C $Id: trbufr.h,v 1.5 2008-07-27 03:55:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      COMMON /TRBUFR/ METBIT, MBUFER, MBFSAV, METREC, MRECLN,
     1                MOPRST, MCONID
      INTEGER MNWRDS, RECLOC, RECSIZ, MBITST, MBUFOF,
     1        MINSBD 
      PARAMETER (MNWRDS=360, RECLOC=0 , RECSIZ=16, 
     1           MBITST=32 , MBUFOF=32, MINSBD=16)
      INTEGER METBIT, MBUFER(MNWRDS),  MBFSAV(MNWRDS), METREC, MRECLN, 
     1        MOPRST, MCONID
