C
C $Id: mdrgdi.f,v 1.3 2001-11-16 00:27:01 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
