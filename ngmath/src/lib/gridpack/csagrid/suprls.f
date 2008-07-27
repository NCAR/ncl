C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SUPRLS (I,ROWI,N,BI,A,NN,SOLN,ERR,IER)
      DIMENSION       ROWI(N)    ,A(NN)      ,SOLN(N)
      SAVE
C
      IER = 0
      IF (I .GT. 1) GO TO 101
C
C  Routine entered with I.LE.0 means complete the reduction and store
C  the solution in SOLN.
C
      IF (I .LE. 0) GO TO 125
C
C  Set up quantities on first call.
C
      IOLD = 0
      NP1 = N+1
C
C  Compute how many rows can be input now.
C
      L = NN/NP1
      ILAST = 0
      IL1 = 0
      K = 0
      K1 = 0
      ERRSUM = 0.
      NREQ = ((N+5)*N+2)/2
C
C  Error exit if insufficient scratch storage provided.
C
      IF (NN .GE. NREQ) GO TO 101
      IER = 32
      CALL CFAERR (IER,' SUPRLS - insufficient scratch storage provided. 
     + at least ((N+5)*N+2)/2 locations needed',88)
      RETURN
C
C  Store the row in the scratch storage.
C
  101 CONTINUE
C
C  Error exit if (I-IOLD).NE.1.
C
      IF ((I-IOLD) .EQ. 1) GO TO 102
      IER = 35
      CALL CFAERR (IER,' SUPRLS - values of I not in sequence',37)
      RETURN
C
  102 CONTINUE
      IOLD = I
      DO 103 J=1,N
         ILJ = ILAST+J
         A(ILJ) = ROWI(J)
  103 CONTINUE
      ILNP = ILAST+NP1
      A(ILNP) = BI
      ILAST = ILAST+NP1
      ISAV = I
      IF (I .LT. L) RETURN
  104 CONTINUE
      IF (K .EQ. 0) GO TO 115
      K1 = MIN0(K,N)
      IDIAG = -NP1
      IF (L-K .EQ. 1) GO TO 110
C
C  Apply householder transformations to zero out new rows.
C
      DO 109 J=1,K1
         IDIAG = IDIAG+(NP1-J+2)
         I1 = IL1+J
         I2 = I1+NP1*(L-K-1)
         S = A(IDIAG)*A(IDIAG)
         DO 105 II=I1,I2,NP1
            S = S+A(II)*A(II)
  105    CONTINUE
         IF (S .EQ. 0.) GO TO 109
         TEMP = A(IDIAG)
         A(IDIAG) = SQRT(S)
         IF (TEMP .GT. 0.) A(IDIAG) = -A(IDIAG)
         TEMP = TEMP-A(IDIAG)
         TEMP1 = 1./(TEMP*A(IDIAG))
         JP1 = J+1
         DO 108 J1=JP1,NP1
            JDEL = J1-J
            IDJ = IDIAG+JDEL
            S = TEMP*A(IDJ)
            DO 106 II=I1,I2,NP1
               IIJD = II+JDEL
               S = S+A(II)*A(IIJD)
  106       CONTINUE
            S = S*TEMP1
            A(IDJ) = A(IDJ)+S*TEMP
            DO 107 II=I1,I2,NP1
               IIJD = II+JDEL
               A(IIJD) = A(IIJD)+S*A(II)
  107       CONTINUE
  108    CONTINUE
  109 CONTINUE
      GO TO 113
C
C  Apply rotations to zero out the single new row.
C
  110 DO 112 J=1,K1
         IDIAG = IDIAG+(NP1-J+2)
         I1 = IL1+J
         IF (ABS(A(I1)) .LE. 1.E-18) THEN
           S = SQRT(A(IDIAG)*A(IDIAG))
         ELSE if (ABS(A(IDIAG)) .LT. 1.E-18) THEN
           S = SQRT(A(I1)*A(I1))
         ELSE
           S = SQRT(A(IDIAG)*A(IDIAG)+A(I1)*A(I1))
         ENDIF
         IF (S .EQ. 0.) GO TO 112
         TEMP = A(IDIAG)
         A(IDIAG) = S
         S = 1./S
         CN = TEMP*S
         SN = A(I1)*S
         JP1 = J+1
         DO 111 J1=JP1,NP1
            JDEL = J1-J
            IDJ = IDIAG+JDEL
            TEMP = A(IDJ)
            I1JD = I1+JDEL
            A(IDJ) = CN*TEMP+SN*A(I1JD)
            A(I1JD) = -SN*TEMP+CN*A(I1JD)
  111    CONTINUE
  112 CONTINUE
  113 IF (K .LT. N) GO TO 115
      LMKM1 = L-K
C
C  Accumulate residual sum of squares.
C
      DO 114 II=1,LMKM1
         ILNP = IL1+II*NP1
         ERRSUM = ERRSUM+A(ILNP)*A(ILNP)
  114 CONTINUE
      IF (I .LE. 0) GO TO 127
      K = L
      ILAST = IL1
C
C  Determine how many new rows may be input on next iteration.
C
      L = K+(NN-ILAST)/NP1
      RETURN
  115 K11 = K1+1
      K1 = MIN0(L,N)
      IF (L-K .EQ. 1) GO TO 122
      K1M1 = K1-1
      IF (L .GT. N) K1M1 = N
      I1 = IL1+K11-NP1-1
C
C  Perform householder transformations to reduce rows to upper 
C  triangular form.
C
      DO 120 J=K11,K1M1
         I1 = I1+(NP1+1)
         I2 = I1+(L-J)*NP1
         S = 0.
         DO 116 II=I1,I2,NP1
            S = S+A(II)*A(II)
  116    CONTINUE
         IF (S .EQ. 0.) GO TO 120
         TEMP = A(I1)
         A(I1) = SQRT(S)
         IF (TEMP .GT. 0.) A(I1) = -A(I1)
         TEMP = TEMP-A(I1)
         TEMP1 = 1./(TEMP*A(I1))
         JP1 = J+1
         I11 = I1+NP1
         DO 119 J1=JP1,NP1
            JDEL = J1-J
            I1JD = I1+JDEL
            S = TEMP*A(I1JD)
            DO 117 II=I11,I2,NP1
               IIJD = II+JDEL
               S = S+A(II)*A(IIJD)
  117       CONTINUE
            S = S*TEMP1
            I1JD = I1+JDEL
            A(I1JD) = A(I1JD)+S*TEMP
            DO 118 II=I11,I2,NP1
               IIJD = II+JDEL
               A(IIJD) = A(IIJD)+S*A(II)
  118       CONTINUE
  119    CONTINUE
  120 CONTINUE
      IF (L .LE. N) GO TO 122
      NP1MK = NP1-K
      LMK = L-K
C
C  Accumulate residual sum of squares.
C
      DO 121 II=NP1MK,LMK
         ILNP = IL1+II*NP1
         ERRSUM = ERRSUM+A(ILNP)*A(ILNP)
  121 CONTINUE
  122 IMOV = 0
      I1 = IL1+K11-NP1-1
C
C  Squeeze the unnecessary elements out of scratch storage to 
C  allow space for more rows.
C
      DO 124 II=K11,K1
         IMOV = IMOV+(II-1)
         I1 = I1+NP1+1
         I2 = I1+NP1-II
         DO 123 III=I1,I2
            IIIM = III-IMOV
            A(IIIM) = A(III)
  123    CONTINUE
  124 CONTINUE
      ILAST = I2-IMOV
      IL1 = ILAST
      IF (I .LE. 0) GO TO 127
      K = L
C
C  Determine how many new rows may be input on next iteration.
C
      L = K+(NN-ILAST)/NP1
      RETURN
C
C  Complete reduction and store solution in SOLN.
C
  125 L = ISAV
C
C  Error exit if L less than N.
C
      IF (L .GE. N) GO TO 126
      IER = 33
      CALL CFAERR (IER,' SUPRLS - array has too few rows.',33)
      RETURN
  126 CONTINUE
C
C  K.NE.ISAV means further reduction needed.
C
      IF (K .NE. ISAV) GO TO 104
  127 ILAST = (NP1*(NP1+1))/2-1
      IF (A(ILAST-1) .EQ. 0.) GO TO 130
C
C Solve triangular system into ROWI.
C
      SOLN(N) = A(ILAST)/A(ILAST-1)
      DO 129 II=2,N
         IIM1 = II-1
         ILAST = ILAST-II
         S = A(ILAST)
         DO 128 K=1,IIM1
            ILK = ILAST-K
            NPK = NP1-K
            S = S-A(ILK)*SOLN(NPK)
  128    CONTINUE
         ILII = ILAST-II
         IF (A(ILII) .EQ. 0.) GO TO 130
         NPII = NP1-II
         SOLN(NPII) = S/A(ILII)
  129 CONTINUE
C
C  Store residual norm.
C
      ERR = SQRT(ERRSUM)
      RETURN
C
C  Error return if system is singular.
C
  130 CONTINUE
      IER = 34
      CALL CFAERR (IER,' SUPRLS - system is singular.',29)
      RETURN
C
      END
