C
C $Id: mputfd.f,v 1.3 2000-07-12 16:23:50 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MPUTFD (RLAT,RLON,UVAL,VVAL)
C
C Given RLAT and RLON, in degrees, return UVAL and VVAL, in meters.
C
        DOUBLE PRECISION RLAT,RLON,UVAL,VVAL
C
C Declare common blocks required to communicate with USGS code.
C
        COMMON /ERRMZ0/ IERR
        SAVE   /ERRMZ0/
C
        COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
          DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
          INTEGER IPRF
        SAVE   /USGSC1/
C
C Declare the USGS "BLOCK DATA" external to force it to load.
C
        EXTERNAL GTPZBD
C
C Declare required double-precision variables.
C
        DOUBLE PRECISION CRDI(2),CRDO(2),CRDT(2),DTST
C
C Declare a required double-precision function.
C
        DOUBLE PRECISION MPGDDP
C
C Define a couple of conversion constants.
C
        DOUBLE PRECISION DTOR,RTOD
C
        DATA DTOR / .017453292519943D0 /
        DATA RTOD / 57.2957795130823D0 /
C
C If the USGS package is initialized, zero the error flag; otherwise,
C take an error exit.
C
        IF (IPRF.GE.3.AND.IPRF.LE.23) THEN
          IERR=0
        ELSE
          IERR=1
          UVAL=1.D12
          VVAL=1.D12
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
  103   CALL PJ03DP (CRDI,CRDO,0)
        GO TO 201
C
  104   CALL PJ04DP (CRDI,CRDO,0)
        GO TO 201
C
  105   CALL PJ05DP (CRDI,CRDO,0)
        GO TO 201
C
  106   CALL PJ06DP (CRDI,CRDO,0)
        GO TO 201
C
  107   DTST=ABS(RLON-UTPA(5))
C
        IF (DTST.LE.90.D0.OR.ABS(DTST-360.D0).LE.90.D0) THEN
          CALL PJ07DP (CRDI,CRDO,0)
        ELSE
          IERR=1
        END IF
C
        GO TO 201
C
  108   CALL PJ08DP (CRDI,CRDO,0)
        GO TO 201
C
  109   CALL PJ09DP (CRDI,CRDO,0)
        GO TO 201
C
  110   CALL PJ10DP (CRDI,CRDO,0)
        GO TO 201
C
  111   CALL PJ11DP (CRDI,CRDO,0)
        GO TO 201
C
  112   CALL PJ12DP (CRDI,CRDO,0)
        GO TO 201
C
  113   CALL PJ13DP (CRDI,CRDO,0)
        GO TO 201
C
  114   CALL PJ14DP (CRDI,CRDO,0)
        GO TO 201
C
  115   CALL PJ15DP (CRDI,CRDO,0)
        GO TO 201
C
  116   CALL PJ16DP (CRDI,CRDO,0)
        GO TO 201
C
  117   CALL PJ17DP (CRDI,CRDO,0)
        GO TO 201
C
  118   CALL PJ18DP (CRDI,CRDO,0)
        GO TO 201
C
  119   CALL PJ19DP (CRDI,CRDO,0)
        GO TO 201
C
  120   CALL PJ20DP (CRDI,CRDO,0)
        GO TO 201
C
  121   CALL PJ21DP (CRDI,CRDO,0)
        GO TO 201
C
  122   CALL PJ22DP (CRDI,CRDO,0)
        CALL PJ22DP (CRDO,CRDT,1)
C
        IF (IERR.EQ.0) THEN
          IF (MPGDDP(RTOD*CRDI(2),RTOD*CRDI(1),
     +               RTOD*CRDT(2),RTOD*CRDT(1)).GT.1.D-2) THEN
            IERR=1
          END IF
        END IF
C
        GO TO 201
C
  123   CALL PJ23DP (CRDI,CRDO,0)
        GO TO 201
C
  201   IF (IERR.EQ.0) THEN
          UVAL=CRDO(1)
          VVAL=CRDO(2)
        ELSE
          UVAL=1.D12
          VVAL=1.D12
        END IF
C
        RETURN
C
      END
