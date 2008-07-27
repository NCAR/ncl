C
C $Id: mdutfs.f,v 1.3 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDUTFS (RLAT,RLON,UVAL,VVAL)
C
C Given RLAT and RLON, in degrees, return UVAL and VVAL, in meters.
C
        REAL RLAT,RLON,UVAL,VVAL
C
C Declare common blocks required to communicate with USGS code.
C
        COMMON /ERRMZ0/ IERR
        SAVE   /ERRMZ0/
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER IPRF
        SAVE   /USGSC1/
C
C Declare coordinate arrays.
C
        DIMENSION CRDI(2),CRDO(2),CRDT(2)
C
C Declare a required single-precision function.
C
        REAL MDGDSP
C
C Define a couple of conversion constants.
C
        DATA DTOR / .017453292519943E0 /
        DATA RTOD / 57.2957795130823E0 /
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL GTPZBD
C
C If the USGS package is initialized, zero the error flag; otherwise,
C take an error exit.
C
        IF (IPRF.GE.3.AND.IPRF.LE.23) THEN
          IERR=0
        ELSE
          IERR=1
          UVAL=1.E12
          VVAL=1.E12
          RETURN
        END IF
C
C Generate coordinates for projection routine.
C
        CRDI(1)=DTOR*RLON
        CRDI(2)=DTOR*RLAT
C
        GO TO (        103,104,105,106,107,108,109,110,
     +         111,112,113,114,115,116,117,118,119,120,
     +         121,122,123                             ) , IPRF-2
C
  103   CALL PJ03SP (CRDI,CRDO,0)
        GO TO 201
C
  104   CALL PJ04SP (CRDI,CRDO,0)
        GO TO 201
C
  105   CALL PJ05SP (CRDI,CRDO,0)
        GO TO 201
C
  106   CALL PJ06SP (CRDI,CRDO,0)
        GO TO 201
C
  107   DTST=ABS(RLON-REAL(UTPA(5)))
C
        IF (DTST.LE.90..OR.ABS(DTST-360.).LE.90.) THEN
          CALL PJ07SP (CRDI,CRDO,0)
        ELSE
          IERR=1
        END IF
C
        GO TO 201
C
  108   CALL PJ08SP (CRDI,CRDO,0)
        GO TO 201
C
  109   CALL PJ09SP (CRDI,CRDO,0)
        GO TO 201
C
  110   CALL PJ10SP (CRDI,CRDO,0)
        GO TO 201
C
  111   CALL PJ11SP (CRDI,CRDO,0)
        GO TO 201
C
  112   CALL PJ12SP (CRDI,CRDO,0)
        GO TO 201
C
  113   CALL PJ13SP (CRDI,CRDO,0)
        GO TO 201
C
  114   CALL PJ14SP (CRDI,CRDO,0)
        GO TO 201
C
  115   CALL PJ15SP (CRDI,CRDO,0)
        GO TO 201
C
  116   CALL PJ16SP (CRDI,CRDO,0)
        GO TO 201
C
  117   CALL PJ17SP (CRDI,CRDO,0)
        GO TO 201
C
  118   CALL PJ18SP (CRDI,CRDO,0)
        GO TO 201
C
  119   CALL PJ19SP (CRDI,CRDO,0)
        GO TO 201
C
  120   CALL PJ20SP (CRDI,CRDO,0)
        GO TO 201
C
  121   CALL PJ21SP (CRDI,CRDO,0)
        GO TO 201
C
  122   CALL PJ22SP (CRDI,CRDO,0)
        CALL PJ22SP (CRDO,CRDT,1)
C
        IF (IERR.EQ.0) THEN
          IF (MDGDSP(RTOD*CRDI(2),RTOD*CRDI(1),
     +               RTOD*CRDT(2),RTOD*CRDT(1)).GT.1.E-2) THEN
            IERR=1
          END IF
        END IF
C
        GO TO 201
C
  123   CALL PJ23SP (CRDI,CRDO,0)
        GO TO 201
C
  201   IF (IERR.EQ.0) THEN
          UVAL=CRDO(1)
          VVAL=CRDO(2)
        ELSE
          UVAL=1.E12
          VVAL=1.E12
        END IF
C
        RETURN
C
      END
