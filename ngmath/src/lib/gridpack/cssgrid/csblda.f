C
C	$Id: csblda.f,v 1.5 2000-08-22 15:19:07 haley Exp $
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
      BLOCKDATA CSBLDA
C
C  Initialize data.
C
      include 'cscomn.h'
C
C  Number of nodes to use in least squares fit.
C
      DATA NUMLS/10/
C
C  Value for constant SIGMA.
C
      DATA USSIG/1.0D0/
C
C  Tolerance to use in calculating gradient differences to terminate
C  iteration sequence when computing global gradients.
C
      DATA TOLIC/0.01D0/
C
C  Tolerance to use in determining how close each SIGMA element 
C  should be to its optimum value in GETSIG.
C
      DATA TOLSG/0.01D0/
C
C  Fill value to use with NCL functions that can return a missing
C  value.
C
      DATA RMVAL/-8.D0/
C
C  Maximum number of iterations to use in computing SIGMA array.
C
      DATA NUMIT/10/
C
C  Flag to revert to calculating a SIGMA array rather than
C  use a constant SIGMA.  Using a constant SIGMA is effected
C  by setting a value for SIGME using CSSETR.  If ICSIG is 0,
C  then a SIGMA array is used, otherwise a constant SIGMA is used.
C
      DATA ICSIG/0/
C
C  Flag indicating global/local gradients.
C
      DATA IGFLG/1/
C
      END
