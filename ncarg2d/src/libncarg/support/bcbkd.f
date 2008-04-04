C
C $Id: bcbkd.f,v 1.4 2008-04-04 21:02:55 kennison Exp $
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
