C
C $Id: bcbkd.f,v 1.5 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE BCBKD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA BCBKDX
C
C  Data for the Bezier curve package.
C
      include 'bccom.h'
C
C  FRATIO  --  The tolerance limit for the recursive subdivision
C              algorithm.  This limit specifies how close the
C              interpolated curve must be to the actual Bezier
C              curve before subdivision ceases.  This ratio is 
C              specified as a ratio of the maximum screen height
C              (the maximum Y extent in user space that can be mapped 
C              onto the unit interval in NDC space).  It is applied 
C              to the current user space to get a value in user 
C              coordinates.  As implemented the subdivision will 
C              cease after eight levels under any circumstance.
C  NPPC    --  A flag to indicate whether the recursive subdivision
C              algorithm will be overridden.  If NPPC .LT. 2 (the
C              default), then recursive subdivision will be utilized; 
C              if NPPC.GE.2 and NPPC.LE.128, then that many points 
C              will be returned along the curve at equally spaced 
C              values for the parameter in the parametric definition 
C              of the curve.
C      
      DATA FRATIO/.00003/
C
C  Flag for the number of points on a curve.
C
      DATA NPPC/0/
C
      END
