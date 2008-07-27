C
C	$Id: conbd.f,v 1.5 2008-07-27 00:16:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CONBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA CONBDX
      COMMON /CONRE1/ IOFFP      ,SPVAL
      COMMON /CONRE2/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(2000)   ,NR
      COMMON /CONRE4/ ISIZEL     ,ISIZEM     ,ISIZEP     ,NREP       ,
     1                NCRT       ,ILAB       ,NULBLL     ,IOFFD      ,
     2                EXT        ,IOFFM      ,ISOLID     ,NLA        ,
     3                NLM        ,XLT        ,YBT        ,SIDE
      COMMON /RECINT/ IRECMJ     ,IRECMN     ,IRECTX
      DATA IOFFP,SPVAL/0,0.0/
      DATA ISIZEL,ISIZEM,ISIZEP,NLA,NLM,XLT,YBT,SIDE,ISOLID,NREP,NCRT/
     1       1,  2,   0, 16, 40,.05,.05,  .9,  1023,   6,   4 /
      DATA EXT,IOFFD,NULBLL,IOFFM,ILAB/.25,0,3,0,1/
      DATA INX(1),INX(2),INX(3),INX(4),INX(5),INX(6),INX(7),INX(8)/
     1        -1 ,   -1 ,    0 ,    1 ,    1 ,    1 ,    0 ,   -1 /
      DATA INY(1),INY(2),INY(3),INY(4),INY(5),INY(6),INY(7),INY(8)/
     1         0 ,    1 ,    1 ,    1 ,    0 ,   -1 ,   -1 ,   -1 /
      DATA NR/2000/
      DATA IRECMJ,IRECMN,IRECTX/ 1 ,   1 ,   1/
C
C REVISION HISTORY---
C
C JANUARY 1980     ADDED REVISION HISTORY AND CHANGED LIBRARY NAME
C                  FROM CRAYLIB TO PORTLIB FOR MOVE TO PORTLIB
C
C MAY 1980         ARRAYS IWORK AND ENCSCR, PREVIOUSLY TOO SHORT FOR
C                  SHORT-WORD-LENGTH MACHINES, LENGTHENED.  SOME
C                  DOCUMENTATION CLARIFIED AND CORRECTED.
C
C JUNE 1984        CONVERTED TO FORTRAN 77 AND TO GKS
C
C JUNE 1985        ERROR HANDLING LINES ADDED; IF OVERFLOW HAPPENS TO
C                  WORK ARRAY IN STLINE, A WARNING MESSAGE IS WRITTEN
C                  BOTH ON PLOT FRAME AND ON STANDARD ERROR MESSAGE.
C
C NOVEMBER 1989    MADE IR 2000 WORDS LONG INSTEAD OF 500.
C
C MARCH 1990       FIXED THE USE OF SET CALLS.
C
C-------------------------------------------------------------------
C
      END
