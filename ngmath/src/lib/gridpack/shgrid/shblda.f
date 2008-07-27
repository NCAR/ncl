C
C	$Id: shblda.f,v 1.5 2008-07-27 03:10:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SHBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA SHBLDAX
C
C  Initialize data.
C
      COMMON /SHCOMI/ NMLSTQ, NMINFL, NMCELS
C
C  Number of data points used in least squares fit must be initialized
C  at run time - set flag.
C
      DATA NMLSTQ/-1/
C
C  Number of nodes within the radius of influence must be initialized
C  at run time - set flag.
C
      DATA NMINFL/-1/
C
C  Number of rows, columns, planes in the cell grid must be initialized
C  at run time - set flag.
C
      DATA NMCELS/-1/
C
      END
