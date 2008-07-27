C
C	$Id: stline.f,v 1.4 2008-07-27 00:23:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE STLINE (Z,LL,MM,NN,CONV)
      SAVE
      DIMENSION       Z(LL,NN)
C
C THIS ROUTINE FINDS THE BEGINNINGS OF ALL CONTOUR LINES AT LEVEL CONV.
C FIRST THE EDGES ARE SEARCHED FOR LINES INTERSECTING THE EDGE (OPEN
C LINES) THEN THE INTERIOR IS SEARCHED FOR LINES WHICH DO NOT INTERSECT
C THE EDGE (CLOSED LINES).  BEGINNINGS ARE STORED IN IR TO PREVENT RE-
C TRACING OF LINES.  IF IR IS FILLED, THE SEARCH IS STOPPED FOR THIS
C CONV.
C
      COMMON /CONRE2/ IX         ,IY         ,IDX        ,IDY        ,
     1                IS         ,ISS        ,NP         ,CV         ,
     2                INX(8)     ,INY(8)     ,IR(2000)   ,NR
      COMMON /CONRE3/ IXBITS     ,IYBITS
C
      IXYPAK(IXX,IYY) = ISHIFT(IXX,IYBITS)+IYY
C
      L = LL
      M = MM
      N = NN
      CV = CONV
      NP = 0
      ISS = 0
      DO 102 IP1=2,M
         I = IP1-1
         IF (Z(I,1).GE.CV .OR. Z(IP1,1).LT.CV) GO TO 101
         IX = IP1
         IY = 1
         IDX = -1
         IDY = 0
         IS = 1
         CALL CRDRLN (Z,L,M,N)
  101    IF (Z(IP1,N).GE.CV .OR. Z(I,N).LT.CV) GO TO 102
         IX = I
         IY = N
         IDX = 1
         IDY = 0
         IS = 5
         CALL CRDRLN (Z,L,M,N)
  102 CONTINUE
      DO 104 JP1=2,N
         J = JP1-1
         IF (Z(M,J).GE.CV .OR. Z(M,JP1).LT.CV) GO TO 103
         IX = M
         IY = JP1
         IDX = 0
         IDY = -1
         IS = 7
         CALL CRDRLN (Z,L,M,N)
  103    IF (Z(1,JP1).GE.CV .OR. Z(1,J).LT.CV) GO TO 104
         IX = 1
         IY = J
         IDX = 0
         IDY = 1
         IS = 3
         CALL CRDRLN (Z,L,M,N)
  104 CONTINUE
      ISS = 1
      DO 108 JP1=3,N
         J = JP1-1
         DO 107 IP1=2,M
            I = IP1-1
            IF (Z(I,J).GE.CV .OR. Z(IP1,J).LT.CV) GO TO 107
            IXY = IXYPAK(IP1,J)
            IF (NP .EQ. 0) GO TO 106
            DO 105 K=1,NP
               IF (IR(K) .EQ. IXY) GO TO 107
  105       CONTINUE
  106       NP = NP+1
            IF (NP .GT. NR) THEN
C
C THIS PRINTS AN ERROR MESSAGE IF THE LOCAL ARRAY IR IN SUBROUTINE
C STLINE HAS AN OVERFLOW
C THIS MESSAGE IS WRITTEN BOTH ON THE FRAME AND ON THE STANDARD ERROR
C UNIT
C
      IUNIT = I1MACH(4)
      WRITE(IUNIT,1000)
 1000 FORMAT(
     1' WARNING FROM ROUTINE STLINE IN CONREC--WORK ARRAY OVERFLOW')
      CALL GETSET(VXA,VXB,VYA,VYB,XA,XB,YA,YB,LTYPE)
      X = (VXB + VXA) / 2.
      Y = (VYB + VYA) / 2.
      CALL PWRIT(CFUX(X),CFUY(Y),
     1'**WARNING--PICTURE INCOMPLETE**',
     2 31,3,0,0)
      Y = Y - .05
      CALL PWRIT(CFUX(X),CFUY(Y),
     1'WORK ARRAY OVERFLOW IN STLINE',
     2 29,3,0,0)
        RETURN
        ENDIF
            IR(NP) = IXY
            IX = IP1
            IY = J
            IDX = -1
            IDY = 0
            IS = 1
            CALL CRDRLN (Z,L,M,N)
  107    CONTINUE
  108 CONTINUE
      RETURN
      END
