c -----------------------------------------------------
C NCLFORTSTART
      SUBROUTINE RHOMBTRUNC( M, N, Ar, Ai, R )

C NCL:  ab = rhomb_trunC(ab,R) or procedure:   rhomb_trunc (Ar,Ai,R)  

c original code was f90 from Dave Stepaniack
c /home/dataproc/davestep/SPHERE/SUBR_RHOMBOIDAL.f90

c Produces rhomboidal truncation R of an MxN array A of spectral coefficients
c (with the restriction that  1 < R < N).

c Example; A is 18x20:
c (Let m represent zonal wavenumber, and n total wavenumber.)

c m, n->                                                                         N
c |
c v
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c M 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7

c Example; A truncated at R9:

c    1   2   3   4   5   6   7   8   9
  
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

      IMPLICIT NONE

      INTEGER M       ! Extent of zonal wave  number dimension.
      INTEGER N       ! Extent of total wave  number dimension.
      INTEGER R       ! Rhomboidal truncation parameter. R is the
                      ! constant number of total wave numbers to keep.
      REAL    Ar(M,N) ! Spectral coefficients (real).
      REAL    Ai(M,N) ! Spectral coefficients (real).
C NCLEND

      INTEGER im      ! DO loop control variable for zonal wavenumber index.
      INTEGER jn      ! Total wavenumber place holder. 
      INTEGER km      ! Zonal wavenumber place holder.

      INTEGER I,J     ! local loop variables

      ! Check immediately to determine if R is an element of [1,N]:

      IF (R.eq.0) then      ! djs
          WRITE (*,"('SUB RHOMBOIDAL: R=0 no action taken')")
          return
      END IF

      IF ( .NOT. ( ( 1 .lt. R ) .AND. ( R .lt. N ) ) ) THEN
c c c IF ( .NOT. ( ( 1 < R )    .AND. ( R < N ) ) ) THEN

       WRITE (*,'(A25,I3,A7,I2)') "R outside of OPEN set (1,
     +                            ", N, "); R = ", R
       WRITE (*,'(A43)') "Execution stopped in SUBROUTINE RHOMBOIDAL."
       RETURN

      END IF

      ! Now zero out A according to rhomboidal truncation:
      
      IF ( M .ge. N ) THEN
c c c IF ( M >= N ) THEN

        ! Case M greater than or equal to N. 

        DO im = 1, N - R           ! Loop over zonal wavenumber (i.e. rows).

          jn = im

c c c     A( im, jn+R:N ) = 0.0    ! Zero out above diagonal band of width R.
          do j=jn+R,N
             Ar(im,j) = 0.0
             Ai(im,j) = 0.0
          end do

          km = im + 1

c c c     A( km, 1:jn ) = 0.0      ! Zero out below diagonal band of width R.
          do j=1,jn
             Ar(km,j) = 0.0
             Ai(km,j) = 0.0
          end do

        END DO

      ELSE IF ( M .lt. N ) THEN
c c c ELSE IF ( M < N ) THEN

        ! Case M less than N.

        im = 0

c c c   DO 
        DO WHILE ( ((im + R - 1) .ge. N) .OR. (im .gt. (M - 1)) ) 

          im = im + 1
          ! Loop over zonal wavenumber (i.e. rows).

c c c     IF ( ( (im + R - 1) >= N ) .OR. ( im > (M - 1) ) ) EXIT

          jn = im

c c c     A( im, jn+R:N ) = 0.0    ! Zero out above diagonal band of width R.
          do j=jn+R,N
             Ar(im,j) = 0.0
             Ai(im,j) = 0.0
          end do
          

          km = im + 1

c c c     A( km, 1:jn ) = 0.0      ! Zero out below diagonal band of width R.
          do j=1,jn
             Ar(km,j) = 0.0
             Ai(km,j) = 0.0
          end do

        END DO

        IF ( (jn+R+1) .le. N ) THEN
c c c   IF ( (jn+R+1) <= N ) THEN

c c c     A( km, jn+R+1:N ) = 0.0  ! Zero out last row ABOVE diagonal.
          do j=jn+R+1,N
             Ar(km,j) = 0.0
             Ai(km,j) = 0.0
          end do

        END IF

      END IF

      IF ( (km + 1) .le. M ) THEN
c c c IF ( (km + 1) <= M ) THEN
c c c   A(km+1:M,:) = 0.           ! Zero out any remaining rows (if any).
        do i=km+1,M
         do j=1,N
            Ar(i,j) = 0.0
            Ai(i,j) = 0.0
         end do
        end do
      END IF

      return
      end
c     END SUBROUTINE RHOMBOIDAL
c ---------------------------------------------------------
      SUBROUTINE TRITRUNC( M, N, Ar, Ai, T )

c NCL: ab = tri_trunc (ab, T)

c Produces triangular truncation T of MxN spectral coefficient arrays Ar and Ai
c (with the restriction that  1 < T < N-1 if M >=N  or 1 < T < M-1 if M < N).
c                               -   -                    -   -
c Ar contains the real part and Ai the corresponding imaginary part of the
c spectral coefficients. 

c Convention: 

c Sectral coefficients Ar, Ai, are ordered m by n where
c m is the zonal wavenumber and n is the total wave number.
c For Gaussian grids the extents of the m and n dimensions
c are nlat for both dimensions.

c The spectral coefficent Ar(1,1) repesents zonal wavenumber
c 0 and total wavenumber 0; the spectral coefficient
c Ar(nlat,nlat) represents zonal wavenumber nlat-1 and
c total wavenumber nlat-1. Similarly for Ai.

c Example; Ar and Ai are 18x18:
c (Let m represent zonal wavenumber, and n total wavenumber.)

c m, n->                                                                  N
c |
c v
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7
c M 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7

c Example; Ar and Ai truncated at T12:

c   (0)  1   2   3   4   5   6   7   8   9  10  11  12   
  
c   7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 7.7 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 7.7 0.0 0.0 0.0 0.0 0.0 
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 7.7 0.0 0.0 0.0 0.0 0.0 
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
c   0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

      IMPLICIT NONE

      INTEGER M        ! Extent of zonal wave number dimension.
      INTEGER N        ! Extent of total wave number dimension.
      REAL    Ar(M,N)  ! Real spectral coefficients.
      REAL    Ai(M,N)  ! Real spectral coefficients.
      INTEGER T        ! Triangular truncation parameter.
C NCLEND

      INTEGER im   ! DO loop control variable for zonal wavenumber index.
      INTEGER jn   ! Total wavenumber place holder. 
      INTEGER km   ! Zonal wavenumber place holder.
      INTEGER i,j  ! Loop variables

      IF (T.eq.0) then      ! djs
          WRITE (*,"('SUB TRIANGULAR: T=0 no action taken')")
          return
      END IF

      ! Check immediately to determine if T is a valid truncation:

      IF ( M .ge. N ) THEN
c c c IF ( M >= N ) THEN

        IF ( .NOT. ( ( 1 .le. T ) .AND. ( T .le. N-1 ) ) ) THEN
c c c   IF ( .NOT. ( ( 1 <= T ) .AND. ( T <= N-1 ) ) ) THEN
            WRITE (*,'(A25)'   ) "Error for case M >= N :"
            WRITE (*,'(A28,I3)') "T not in range [1,N-1]; T = ", T
            WRITE (*,'(A43)') "Exec stop in SUBROUTINE TRIANGULAR"
        END IF

      ELSE IF ( M .lt. N ) THEN
c c c ELSE IF ( M < N ) THEN

        IF ( .NOT. ( ( 1 .le. T ) .AND. ( T .le. M-1 ) ) ) THEN
c c c   IF ( .NOT. ( ( 1 <= T ) .AND. ( T <= M-1 ) ) ) THEN
            WRITE (*,'(A25)'   ) "Error for case M < N :"
            WRITE (*,'(A28,I3)') "T not in range [1,N-1]; T = ", T
            WRITE (*,'(A43)') "Exec stop in SUBROUTINE TRIANGULAR"
          RETURN

        END IF

      END IF

      ! Now zero out Ar and Ai according to triangular truncation:

      IF ( M .le. N ) THEN
c c c IF ( M <= N ) THEN             ! Case M less than or equal to N.

       DO im = 1, M - 1              ! Loop over row index.

        jn = im
        km = im + 1

c c c   Ar(km,1:jn) = 0.             ! Zero out elements below diagonal.
c c c   Ai(km,1:jn) = 0.
        do j=1,jn
           Ar(km,j) = 0.0
           Ai(km,j) = 0.0
        end do

       END DO

      ELSE IF ( M .gt. N ) THEN
c c c ELSE IF ( M > N ) THEN

       DO im = 1, N - 1              ! Loop over row index.

        jn = im
        km = im + 1

c c c   Ar(km,1:jn) = 0.             ! Zero out elements below diagonal.
c c c   Ai(km,1:jn) = 0.
        do j=1,jn
           Ar(km,j) = 0.0
           Ai(km,j) = 0.0
        end do
       END DO

        
c c c   Ar(km+1:M,1:jn+1) = 0.       ! (Partially) zero out remaining rows
c c c   Ai(km+1:M,1:jn+1) = 0. 
        do j=1,jn+1
         do i=km+1,M
           Ar(i,j) = 0.0
           Ai(i,j) = 0.0
         end do
        end do

      END IF

      IF ( (T + 2) .le. N ) THEN
c c c IF ( (T + 2) <= N ) THEN ! Truncate columns beyond T+1 (if necessary).

c c c   Ar(:,T+2:N) = 0.
c c c   Ai(:,T+2:N) = 0.
        do j=T+2,N
          do i=1,M
            Ar(i,j) = 0.0
            Ai(i,j) = 0.0
         end do
        end do
      END IF

      return
      end
c c c END SUBROUTINE TRIANGULAR
