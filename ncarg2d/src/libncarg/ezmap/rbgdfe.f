C
C $Id: rbgdfe.f,v 1.1 1999-04-02 22:59:47 kennison Exp $
C
      FUNCTION RBGDFE (RLAT)
C
C The value of RBGDFE(RLAT) is the signed distance, from the equator,
C of the parallel at latitude RLAT; its magnitude is a fraction of the
C length of the equator.
C
        DIMENSION PDFE(19)
C
        DATA PDFE( 1) / 0.0000 /  !   0N
        DATA PDFE( 2) / 0.0620 /  !   5N
        DATA PDFE( 3) / 0.1240 /  !  10N
        DATA PDFE( 4) / 0.1860 /  !  15N
        DATA PDFE( 5) / 0.2480 /  !  20N
        DATA PDFE( 6) / 0.3100 /  !  25N
        DATA PDFE( 7) / 0.3720 /  !  30N
        DATA PDFE( 8) / 0.4340 /  !  35N
        DATA PDFE( 9) / 0.4958 /  !  40N
        DATA PDFE(10) / 0.5571 /  !  45N
        DATA PDFE(11) / 0.6176 /  !  50N
        DATA PDFE(12) / 0.6769 /  !  55N
        DATA PDFE(13) / 0.7346 /  !  60N
        DATA PDFE(14) / 0.7903 /  !  65N
        DATA PDFE(15) / 0.8435 /  !  70N
        DATA PDFE(16) / 0.8936 /  !  75N
        DATA PDFE(17) / 0.9394 /  !  80N
        DATA PDFE(18) / 0.9761 /  !  85N
        DATA PDFE(19) / 1.0000 /  !  90N
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
        RBGDFE=.5072*SIGN((1.-FLAT)*PDFE(ILAT)+FLAT*PDFE(ILAT+1),RLAT)
C
C Done.
C
        RETURN
C
      END
