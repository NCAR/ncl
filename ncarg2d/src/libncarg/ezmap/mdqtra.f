C
C $Id: mdqtra.f,v 1.6 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDQTRA (RLAT,RLON,UVAL,VVAL)
C
        DOUBLE PRECISION RLAT,RLON,UVAL,VVAL
C
C Declare a special common block, containing only those variables that
C MDQINI needs to set to make MDQTRA, MDQTRI, and MDQTRN carry out the
C transformation in effect at the time MDQINI was called.
C
        COMMON /MAQCMN/  ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +                   DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +                   SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +                   UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA,IPRF,
     +                   IPRJ,IROD,ELPM
        DOUBLE PRECISION ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +                   DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +                   SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +                   UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA(15)
C
        INTEGER IPRF,IPRJ,IROD
C
        LOGICAL ELPM
C
        SAVE   /MAQCMN/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDQTRA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C The call to MDQTRA is simply passed on to MDQTRN, but the values
C returned are checked to see if the point lies outside the perimeter;
C if so, the value 1.D12 is substituted for UVAL.
C
        CALL MDQTRN (RLAT,RLON,UVAL,VVAL)
        IF (ICFELL('MDQTRA',2).NE.0) RETURN
C
        IF (ELPM) THEN
          IF (((UVAL-UCNM)/URNM)**2+
     +        ((VVAL-VCNM)/VRNM)**2.GT.1.000002D0) THEN
            UVAL=1.D12
            VVAL=1.D12
          END IF
        ELSE
          IF (UVAL.LT.UMNM.OR.UVAL.GT.UMXM.OR.
     +        VVAL.LT.VMNM.OR.VVAL.GT.VMXM) THEN
            UVAL=1.D12
            VVAL=1.D12
          END IF
        END IF
C
        RETURN
C
      END
