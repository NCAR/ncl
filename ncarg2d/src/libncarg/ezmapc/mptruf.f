C
C $Id: mptruf.f,v 1.1 1999-04-02 23:05:51 kennison Exp $
C
      SUBROUTINE MPTRUF (RLAT,RLON,UVAL,VVAL)
C
C Declare common blocks required to communicate with USGS code.
C
        COMMON /ERRMZ0/ IERR
        SAVE   /ERRMZ0/
C
        COMMON /PROJZ0/ IPRO
        SAVE   /PROJZ0/
C
        COMMON /USGSC1/ IPRF,UTPA(15),UUMN,UUMX,UVMN,UVMX
          DOUBLE PRECISION UTPA
        SAVE   /USGSC1/
C
C Declare required double-precision variables.
C
        DOUBLE PRECISION CRDI(2),CRDO(2),CRDT(2),DTST
C
C Declare a required double-precision function.
C
        DOUBLE PRECISION MPGCAN
C
C Define a couple of conversion constants.
C
        DOUBLE PRECISION DTOR,RTOD
C
        DATA DTOR / .017453292519943D0 /
        DATA RTOD / 57.2957795130823D0 /
C
C Given RLAT and RLON, in degrees, return UVAL and VVAL, in meters.
C
        CRDI(1)=DTOR*DBLE(RLON)
        CRDI(2)=DTOR*DBLE(RLAT)
C
        GO TO (101,102,103,104,105,106,107,108,109,110,
     +         111,112,113,114,115,116,117,118,119,120,
     +         121,122,123                             ) , IPRO
C
  101   CALL PJ01Z0 (CRDI,CRDO,0)
        GO TO 201
C
  102   CALL PJ02Z0 (CRDI,CRDO,0)
        GO TO 201
C
  103   CALL PJ03Z0 (CRDI,CRDO,0)
        GO TO 201
C
  104   CALL PJ04Z0 (CRDI,CRDO,0)
        GO TO 201
C
  105   CALL PJ05Z0 (CRDI,CRDO,0)
        GO TO 201
C
  106   CALL PJ06Z0 (CRDI,CRDO,0)
        GO TO 201
C
  107   DTST=ABS(DBLE(RLON)-UTPA(5))
        IF (DTST.LE.90.D0.OR.ABS(DTST-360.D0).LE.90.D0) THEN
          CALL PJ07Z0 (CRDI,CRDO,0)
        ELSE
          IERR=1
        END IF
        GO TO 201
C
  108   CALL PJ08Z0 (CRDI,CRDO,0)
        GO TO 201
C
  109   CALL PJ09Z0 (CRDI,CRDO,0)
        GO TO 201
C
  110   CALL PJ10Z0 (CRDI,CRDO,0)
        GO TO 201
C
  111   CALL PJ11Z0 (CRDI,CRDO,0)
        GO TO 201
C
  112   CALL PJ12Z0 (CRDI,CRDO,0)
        GO TO 201
C
  113   CALL PJ13Z0 (CRDI,CRDO,0)
        GO TO 201
C
  114   CALL PJ14Z0 (CRDI,CRDO,0)
        GO TO 201
C
  115   CALL PJ15Z0 (CRDI,CRDO,0)
        GO TO 201
C
  116   CALL PJ16Z0 (CRDI,CRDO,0)
        GO TO 201
C
  117   CALL PJ17Z0 (CRDI,CRDO,0)
        GO TO 201
C
  118   CALL PJ18Z0 (CRDI,CRDO,0)
        GO TO 201
C
  119   CALL PJ19Z0 (CRDI,CRDO,0)
        GO TO 201
C
  120   CALL PJ20Z0 (CRDI,CRDO,0)
        GO TO 201
C
  121   CALL PJ21Z0 (CRDI,CRDO,0)
        GO TO 201
C
  122   CALL PJ22Z0 (CRDI,CRDO,0)
        CALL PJ22Z0 (CRDO,CRDT,1)
        IF (IERR.EQ.0) THEN
          IF (MPGCAN(RTOD*CRDI(2),RTOD*CRDI(1),
     +               RTOD*CRDT(2),RTOD*CRDT(1)).GT.1.D-2) THEN
            IERR=1
          END IF
        END IF
        GO TO 201
C
  123   CALL PJ23Z0 (CRDI,CRDO,0)
        GO TO 201
C
  201   IF (IERR.EQ.0) THEN
          UVAL=REAL(CRDO(1))
          VVAL=REAL(CRDO(2))
        ELSE
          UVAL=1.E12
          VVAL=1.E12
        END IF
C
        RETURN
C
      END
