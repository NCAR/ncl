C
C $Id: idblda.f,v 1.3 2000-08-22 15:06:52 haley Exp $
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
      BLOCK DATA IDBLDA
C
C This "routine" provides default values for the internal parameters of
C BIVAR.
C
C The common block IDCOMN holds BIVAR variables associated with changes
C made in October, 1995, including a couple of internal parameters.
C
        COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
        SAVE   /IDCOMN/
C
C INTY is the flag that says what type of interpolation to use (0 for
C quintic interpolation, 1 for linear interpolation).
C
        DATA INTY / 0 /
C
C ITTY is the flag that says what type of triangulation to use (0 for
C the original, 1 for the new).
C
        DATA ITTY / 0 /
C
C ALSP, BLSP, CLSP, XAVG, and YAVG are not actually internal parameters
C of BIVAR.  They are computed and used when linear interpolation is
C selected.  The first three are coefficients in the equation of the
C plane that best fits the data and the final two are mean X and Y
C values computed from the data.
C
        DATA ALSP,BLSP,CLSP,XAVG,YAVG / 5*0. /
C
      END
