C
C $Id: mdilon.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDILON (PLAT,PLON,UCOP,VCOP,
     +                   QLAT,QLON,UCOQ,VCOQ,
     +                        RLON,UCOR,VCOR)
C
C Given the lat/lon and U/V coordinates of two points P and Q and the
C longitude of a point R along the line in the U/V plane connecting the
C points P and Q, this routine returns the U and V coordinates of R,
C found using a binary-halving technique.
C
        DOUBLE PRECISION PLAT,PLON,UCOP,VCOP
        DOUBLE PRECISION QLAT,QLON,UCOQ,VCOQ
        DOUBLE PRECISION      RLON,UCOR,VCOR
C
C Declare local variables.
C
        DOUBLE PRECISION ALAT,ALON,UCOA,VCOA
        DOUBLE PRECISION BLAT,BLON,UCOB,VCOB
        DOUBLE PRECISION CLAT,CLON,UCOC,VCOC
C
        INTEGER          NREP
C
        IF (PLON.LT.QLON) THEN
          ALAT=PLAT
          ALON=PLON
          UCOA=UCOP
          VCOA=VCOP
          BLAT=QLAT
          BLON=QLON
          UCOB=UCOQ
          VCOB=VCOQ
        ELSE
          ALAT=QLAT
          ALON=QLON
          UCOA=UCOQ
          VCOA=VCOQ
          BLAT=PLAT
          BLON=PLON
          UCOB=UCOP
          VCOB=VCOP
        END IF
C
        UCOC=UCOA
        VCOC=VCOA
C
        NREP=0
C
  101   IF (NREP.LT.100) THEN
          IF (ALON.EQ.BLON) GO TO 102
          UCOC=UCOA+((RLON-ALON)/(BLON-ALON))*(UCOB-UCOA)
          VCOC=VCOA+((RLON-ALON)/(BLON-ALON))*(VCOB-VCOA)
          CALL MDPTRI (UCOC,VCOC,CLAT,CLON)
          IF (CLAT.EQ.1.D12) GO TO 102
          IF (CLON.LT.ALON) CLON=CLON+360.D0
          IF (CLON.GT.BLON) CLON=CLON-360.D0
          IF (CLON.LT.ALON.OR.CLON.GT.BLON) GO TO 102
          IF (CLON.LT.RLON) THEN
            ALAT=CLAT
            ALON=CLON
            UCOA=UCOC
            VCOA=VCOC
          ELSE
            BLAT=CLAT
            BLON=CLON
            UCOB=UCOC
            VCOB=VCOC
          END IF
          NREP=NREP+1
          IF (ABS(CLON-RLON).GT..001D0*ABS(QLON-PLON)) GO TO 101
        END IF
C
  102   UCOR=UCOC
        VCOR=VCOC
C
        RETURN
C
      END
