
      PROGRAM CCPLINE
C 
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
      
      PARAMETER (K=9,N=7,LRWK=1000,LIWK=1000,LZDT=2000)
      REAL Z(K,N), ZDAT(LZDT), RWRK(LRWK)
      INTEGER M, IWRK(LIWK)
      
      CALL GETDAT (Z, K, M, N) 
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up a color table
C
      CALL COLOR(IWKID)
C
C Set each contour level value
C
      CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
      CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',13)
      DO 5, I=1,13
         CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',
     +        REAL(I-1)*1000.-500.)
         IF (MOD(I,3).EQ.0) THEN
C
C Make the contour line  dashed
C
            CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
            CALL CPSETI ('CLD - CONTOUR LINE DASH PATTERN', 21845)
         ELSE IF (MOD(I,3).EQ.1) THEN
C
C Make the contour line three times as thick
C
            CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
            CALL CPSETR ('CLL - CONTOUR LINE LINE WIDTH', 3.)
         ELSE IF (MOD(I,3).EQ.2) THEN
C
C Make the contour line  red
C
            CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',I)
            CALL CPSETI ('CLC - CONTOUR LINE COLOR INDEX',2)
         ENDIF
 5    CONTINUE     
C
C Initialize Conpack
C
      CALL CPSPS1(Z,K,M,N,RWRK,LRWK,IWRK,LIWK,ZDAT,LZDT)
C
C Draw perimeter
C
      CALL CPBACK(ZDAT, RWRK, IWRK)
C
C Draw Contours
C
      CALL CPCLDR(ZDAT,RWRK,IWRK)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE GETDAT (Z, K, M, N)
      INTEGER I,J,K,M,N
      REAL Z(K,N)
      
      M=K
      RMIN = 0.
      RMAX = 0.
      DO 10, I=1,M
         DO 20, J=1,N
            Z(I,J)= -16.*REAL(I**2*J) +
     1           34*REAL(I*J**2) - REAL(6*I) + 93.
            IF (RMIN.GT.Z(I,J)) RMIN=Z(I,J)
            IF (RMAX.LT.Z(I,J)) RMAX=Z(I,J)
 20      CONTINUE
 10   CONTINUE
      
      RETURN
      END
      
      SUBROUTINE COLOR(IWKID)
      
      CALL GSCR(IWKID,0,0.,0.,0.)
      CALL GSCR(IWKID,1,1.,1.,1.)
      CALL GSCR(IWKID,2,1.,0.,0.)
      
      RETURN
      END
      
