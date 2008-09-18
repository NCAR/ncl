C
C $Id: mdrgfp.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGFP (XCRA,YCRA,NCRA,ITYP)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C Declare required EZMAP common blocks.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
        IF (NCRA.GT.2.AND.ITYP.GT.0) THEN
          IF (ICSF(ITYP).GE.0) THEN
            NCRM=0
            DO 101 I=1,NCRA
              CALL MAPTRN (OLAT+YCRA(I),OLON+XCRA(I),UVAL,VVAL)
              IF (UVAL.NE.1.E12) THEN
                NCRM=NCRM+1
                XCRA(NCRM)=UVAL
                YCRA(NCRM)=VVAL
              END IF
  101       CONTINUE
            IF (NCRM.GT.2) THEN
              CALL GSFACI (ICSF(ITYP))
              CALL GFA    (NCRM,XCRA,YCRA)
            END IF
          END IF
        END IF
C
        RETURN
C
      END
