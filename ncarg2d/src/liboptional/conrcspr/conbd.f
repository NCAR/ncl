C
C	$Id: conbd.f,v 1.1.1.1 1992-04-17 22:35:08 ncargd Exp $
C
C
C
      BLOCK DATA CONBD
      COMMON /CONRE1/ IOFFP      ,SPVAL      ,IHILO
      COMMON /CONRE2/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(2000)   ,NR
      COMMON /SPRINT/ ISPRMJ     ,ISPRMN     ,ISPRTX
      DATA IOFFP,SPVAL,IHILO/0,0.0,3/
      DATA INX(1),INX(2),INX(3),INX(4),INX(5),INX(6),INX(7),INX(8)/
     1        -1 ,   -1 ,    0 ,    1 ,    1 ,    1 ,    0 ,   -1 /
      DATA INY(1),INY(2),INY(3),INY(4),INY(5),INY(6),INY(7),INY(8)/
     1         0 ,    1 ,    1 ,    1 ,    0 ,   -1 ,   -1 ,   -1 /
      DATA NR/2000/
      DATA ISPRMJ,ISPRMN,ISPRTX/ 1 , 1 , 1 /
C
C
C-------------------------------------------------------------------------
C
C REVISION HISTORY
C
C JUNE 1984                          CONVERTED TO FORTRAN 77 AND GKS
C
C NOVEMBER 1989                      CHANGED THE LENGTH OF IR TO 2000.
C
C MARCH 1990                         CORRECTED USE OF SET CALLS.
C
C------------------------------------------------------------------------
      END
