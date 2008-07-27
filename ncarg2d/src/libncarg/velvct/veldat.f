C
C       $Id: veldat.f,v 1.6 2008-07-27 00:17:34 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
