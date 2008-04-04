C
C       $Id: veldat.f,v 1.5 2008-04-04 21:02:57 kennison Exp $
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
      SUBROUTINE VELDAT
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA VELDATX
C
C THIS 'ROUTINE' DEFINES THE DEFAULT VALUES OF THE VELVCT PARAMETERS.
C
        SAVE
C
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
      DATA     EXT /    0.25 /
      DATA  ICTRFG /    1    /
      DATA    ILAB /    0    /
      DATA   IOFFD /    0    /
      DATA   IOFFM /    0    /
      DATA     RMN /  160.00 /
      DATA     RMX / 6400.00 /
      DATA    SIDE /    0.90 /
      DATA    SIZE /  256.00 /
      DATA     XLT /    0.05 /
      DATA     YBT /    0.05 /
      DATA     ZMX /    0.00 /
      DATA    INCX /    1     /
      DATA    INCY /    1     /
C
C REVISION HISTORY ----------------------------------------------------
C
C FEBRUARY, 1979   ADDED REVISION HISTORY
C                  MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C JULY, 1979       FIXED HI VECTOR TRAP AND MESSAGE INDICATING
C                  MAXIMUM VECTOR PLOTTED.
C
C DECEMBER, 1979   CHANGED THE STATISTICS CALL FROM CRAYLIB TO NSSL
C
C MARCH, 1981      FIXED SOME FRINGE-CASE ERRORS, CHANGED THE CODE TO
C                  USE FL2INTT AND PLOTIT INSTEAD OF MXMY, FRSTPT, AND
C                  VECTOR, AND MADE THE ARROWHEADS NARROWER (45 DEGREES
C                  APART, RATHER THAN 60 DEGREES APART)
C
C FEBRUARY, 1984   PROVIDED A DIMENSION STATEMENT FOR A VARIABLE INTO
C                  WHICH A TEN-CHARACTER STRING WAS BEING ENCODED.  ON
C                  THE CRAY, WHEN THE ENCODE WAS DONE, A WORD FOLLOWING
C                  THE VARIABLE WAS CLOBBERED, BUT THIS APPARENTLY MADE
C                  NO DIFFERENCE.  ON AT LEAST ONE OTHER MACHINE, THE
C                  CODE BLEW UP.  (ERROR REPORTED BY GREG WOODS)
C
C JULY, 1984       CONVERTED TO FORTRAN77 AND GKS.
C
C MARCH, 1990      CORRECTED THE USE OF SET CALLS.
C
C ---------------------------------------------------------------------
      END
