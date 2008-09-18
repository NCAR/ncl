C
C $Id: mdrgdi.f,v 1.9 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGDI (DINM)
C
C This routine is expected to return the name of the directory in which
C the RANGS/GSHHS data files have been placed.
C
        CHARACTER*(*) DINM
C
C The pathname for the directory is obtained from the NCAR Graphics
C routine GNGPAT, which examines various environment variables and
C returns a value for DINM, as follows:
C
C   Environment variable set:   Value returned in DINM:
C   -------------------------   ------------------------------------
C   NCARG_RANGS                 $NCARG_RANGS
C   NCARG_DATABASE              $NCARG_DATABASE/rangs
C   NCARG_NCARG                 $NCARG_NCARG/database/rangs
C   NCARG_LIB                   $NCARG_LIB/ncarg/database/rangs
C   NCARG_ROOT                  $NCARG_ROOT/lib/ncarg/database/rangs
C
C (A user could also elect to supply his or her own version of MDRGDI,
C in which the value of DINM is set directly.)
C
        CALL GNGPAT (DINM,'rangs',ISTA)
C
C If the status parameter is anything other than -1, we have the value
C we need; otherwise, log an error and return.
C
        IF (ISTA.EQ.-1) THEN
          CALL SETER ('MDRGDI - REPLACE ME - RETURN NAME OF DIRECTORY CO
     +NTAINING DATA',1,1)
          DINM='.'
        END IF
C
C Done.
C
        RETURN
C
      END
