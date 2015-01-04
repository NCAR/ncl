C NCLFORTSTART
       subroutine detcalc(a,n,det)
       implicit none
c                        input
       integer n
       double precision a(n,n)
c                        output
       double precision det
C NCLEND
c                        local (dynamic)
       integer indx(n)

       call dtrm(a,n,det,indx)

       return
       end
c=======================================================================

c Updated 10/24/2001.
c
ccccccccccccccccccccccccc     Program 4.2     cccccccccccccccccccccccccc
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                      c
c Please Note:                                                         c
c                                                                      c
c (1) This computer program is part of the book, "An Introduction to   c
c     Computational Physics," written by Tao Pang and published and    c
c     copyrighted by Cambridge University Press in 1997.               c
c                                                                      c
c (2) No warranties, express or implied, are made for this program.    c
c                                                                      c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DTRM(A,N,D,INDX)
      IMPLICIT NONE
C
C Subroutine for evaluating the determinant of a matrix using 
C the partial-pivoting Gaussian elimination scheme.
C
      INTEGER N
      DOUBLE PRECISION A(N,N), D
      INTEGER INDX(N)
C
      INTEGER I, J, MSGN

      CALL ELGS(A,N,INDX)
C
      D    = 1.0D0
      DO I = 1,N
         D = D*A(INDX(I),I)
      END DO   
C
      MSGN = 1
      DO I=1,N
        DO WHILE (I.NE.INDX(I))
           MSGN = -MSGN
           J = INDX(I)
           INDX(I) = INDX(J)
           INDX(J) = J
        END DO
      END DO
      D = MSGN*D
C
      RETURN
      END
C ---
      SUBROUTINE ELGS(A,N,INDX)
      IMPLICIT NONE
      INTEGER N
      DOUBLE PRECISION A(N,N),C(N)
      INTEGER INDX(N)
C
C Subroutine to perform the partial-pivoting Gaussian elimination.
C A(N,N) is the original matrix in the input and transformed
C matrix plus the pivoting element ratios below the diagonal in
C the output.  INDX(N) records the pivoting order.

      INTEGER I, J, K, ITMP
      DOUBLE PRECISION C1, PI1, PI, PJ
C
C Initialize the index
C
      DO I=1,N
        INDX(I) = I
      END DO
C
C Find the rescaling factors, one from each row
C
        DO I=1,N
          C1= 0.0D0
          DO J=1,N
             C1 = AMAX1(C1,ABS(A(I,J)))
          END DO
          C(I) = C1
        END DO
C
C Search the pivoting (largest) element from each column
C
      DO J=1,N-1
         PI1 = 0.0D0
        DO I=J,N
           PI = ABS(A(INDX(I),J))/C(INDX(I))
          IF (PI.GT.PI1) THEN
              PI1 = PI
              K   = I
          ELSE
          ENDIF
        END DO
C
C Interchange the rows via INDX(N) to record pivoting order
C
        ITMP    = INDX(J)
        INDX(J) = INDX(K)
        INDX(K) = ITMP
        DO I=J+1,N
          PJ  = A(INDX(I),J)/A(INDX(J),J)
C
C Record pivoting ratios below the diagonal
C
          A(INDX(I),J) = PJ
C
C Modify other elements accordingly
C
          DO K=J+1,N
            A(INDX(I),K) = A(INDX(I),K)-PJ*A(INDX(J),K)
          END DO
        END DO
      END DO
C
      RETURN
      END
