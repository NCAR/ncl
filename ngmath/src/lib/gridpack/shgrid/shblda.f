C
C	$Id: shblda.f,v 1.3 2000-08-22 15:19:45 haley Exp $
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
      BLOCKDATA SHBLDA
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
