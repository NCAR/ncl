C
C $Id: rbglen.f,v 1.1 1999-04-02 22:59:47 kennison Exp $
C
      FUNCTION RBGLEN (RLAT)
C
C The value of RBGLEN(RLAT) is the length of the parallel at latitude
C RLAT, stated as a fraction of the length of the equator.
C
        DIMENSION PLEN(19)
C
        DATA PLEN( 1) / 1.0000 /  !   0N
        DATA PLEN( 2) / 0.9986 /  !   5N
        DATA PLEN( 3) / 0.9954 /  !  10N
        DATA PLEN( 4) / 0.9900 /  !  15N
        DATA PLEN( 5) / 0.9822 /  !  20N
        DATA PLEN( 6) / 0.9730 /  !  25N
        DATA PLEN( 7) / 0.9600 /  !  30N
        DATA PLEN( 8) / 0.9427 /  !  35N
        DATA PLEN( 9) / 0.9216 /  !  40N
        DATA PLEN(10) / 0.8962 /  !  45N
        DATA PLEN(11) / 0.8679 /  !  50N
        DATA PLEN(12) / 0.8350 /  !  55N
        DATA PLEN(13) / 0.7986 /  !  60N
        DATA PLEN(14) / 0.7597 /  !  65N
        DATA PLEN(15) / 0.7186 /  !  70N
        DATA PLEN(16) / 0.6732 /  !  75N
        DATA PLEN(17) / 0.6213 /  !  80N
        DATA PLEN(18) / 0.5722 /  !  85N
        DATA PLEN(19) / 0.5322 /  !  90N
C
C Determine where the parallel of interest lies relative to the ones
C represented in the tables (between the ones associated with elements
C ILAT and ILAT+1 and a fractional distance FLAT from the former to the
C latter).
C
        ILAT=MAX(1,MIN(18,INT(1.+ABS(RLAT)/5.)))
C
        FLAT=1.+ABS(RLAT)/5.-REAL(ILAT)
C
C Return the desired value.
C
        RBGLEN=(1.-FLAT)*PLEN(ILAT)+FLAT*PLEN(ILAT+1)
C
C Done.
C
        RETURN
C
      END
