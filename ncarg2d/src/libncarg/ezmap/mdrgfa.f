C
C $Id: mdrgfa.f,v 1.10 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGFA (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
C This routine is called by ARSCAM, which has been called by MDRGSX,
C which has been called by MDRGSF.  If the polygon defined by the
C arguments is one that should be filled, the polygon is transformed
C from its position on the unit square to a 1-degree square on a
C lat/lon grid and then to its position on the map defined by the
C current state of EZMAP and then filled.
C
        REAL XCRA(NCRA),YCRA(NCRA)
        INTEGER IAAI(NGPS),IAGI(NGPS),NGPS
C
C Declare required EZMAP common blocks.
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),IDPF,LCRA,NILN,NILT,OLAT,OLON
        INTEGER          ICOL,ICSF,IDPF,LCRA,NILN,NILT
        REAL             OLAT,OLON
        SAVE   /MAPRGD/
C
C Declare local variables.
C
        REAL             UVAL,VVAL,XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,
     +                   YWDT
        INTEGER          LNLG
C
C Because of the way MDRGFA is called, we know that the polygon has but
C one area identifier, relative to edge group identifier 1; if that id
C is greater than 0 and the polygon has at least three points, then we
C transform and fill it.
C
        IF (NCRA.GT.2.AND.IAAI(1).GT.0) THEN
          IF (ICSF(IAAI(1)).GE.0) THEN
            CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
            CALL MDPRS
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
              CALL GSFACI (ICSF(IAAI(1)))
              CALL GFA    (NCRM,XCRA,YCRA)
            END IF
            CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
