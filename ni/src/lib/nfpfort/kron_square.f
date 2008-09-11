C NCLFORTSTART
      SUBROUTINE KRONSQM (n,n2,A,B,C)
        IMPLICIT NONE
c                                                ! input
        INTEGER              n, n2 
        DOUBLE PRECISION     A(n,n), B(n,n)
c                                                ! output
        DOUBLE PRECISION     C(n2,n2)
C NCLEND
c computes the Kronecker product of square matrices A and B (dim=d), 
c puts into C (dim=n^2)
        INTEGER              i, j, k, l, row, col
      
        DO i=1,n
          DO k=1,n
            row=n*(i-1)+k
            DO j=1,n
              DO l=1,n
                col=n*(j-1)+l
                C(row,col)=A(i,j)*B(k,l)
              END DO
            END DO
          END DO
        END DO
      
        RETURN
      END 
