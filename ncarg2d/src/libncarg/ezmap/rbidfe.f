C
C $Id: rbidfe.f,v 1.1 1999-04-02 22:59:48 kennison Exp $
C
      FUNCTION RBIDFE (QDFE)
C
C The value of RBIDFE(QDFE) is the latitude for which the distance of
C the parallel of the Robinson projection from the equator is QDFE.
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
C XDFE is the magnitude of QDFE, limited to the range of values in the
C table.
C
        XDFE=MAX(0.,MIN(1.,ABS(QDFE)/.5072))
C
C Find the indices of the values in the table between which XDFE lies.
C
        IBEG=1
        IEND=19
C
  101   ITST=(IBEG+IEND)/2
C
        IF (PDFE(ITST).LE.XDFE) THEN
          IBEG=ITST
        ELSE
          IEND=ITST
        END IF
C
        IF (IEND-IBEG.GT.1) GO TO 101
C
C Now, just interpolate to find the desired latitude.
C
        RBIDFE=SIGN(5.*(REAL(IBEG-1)+(      XDFE-PDFE(IBEG))/
     +                               (PDFE(IEND)-PDFE(IBEG))),QDFE)
C
C Done.
C
        RETURN
C
      END
