C
C	$Id: gwiiws.f,v 1.4 2000-07-12 16:54:42 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE GWIIWS
C
C  Initialization of constants.
C
C
C  Initialize workstation state list and other internal constants.
C
      include 'gwiwsl.h'
      include 'gwiins.h'
      include 'gwienu.h'
C
C  Internal constants.
C
C  MIN and MAX VDC address limits in the X and Y directions
C  (the VDC extent).
C
      MINXVD = 0
      MAXXVD = 32767
      MINYVD = 0
      MAXYVD = 32767
C
C  NDC to VDC mapping coefficients, X and Y directions.
C
      MXOFF = 0
      MXSCAL = 32767
      MYOFF = 0
      MYSCAL = 32767
C
C  WSL items.
C
C  State is inactive (0=GINACT)
C
      MSTATE = GINACT
C
C  Display surface is empty (1=GEMPTY)
C
      MDEMPT = GEMPTY
C
C  Requested workstation window in NDC is unit square.
C
      RWINDO(1) = 0.0
      RWINDO(2) = 1.0
      RWINDO(3) = 0.0
      RWINDO(4) = 1.0
C
C  Current workstation window in NDC is unit square.
C
      CWINDO(1) = 0.0
      CWINDO(2) = 1.0
      CWINDO(3) = 0.0
      CWINDO(4) = 1.0
C
C  Requested workstation viewport is real address space.
C
      RWKVP(1) = 0.
      RWKVP(2) = 32767.
      RWKVP(3) = 0.
      RWKVP(4) = 32767.
C
C  Current workstation viewport is real address space.
C
      CWKVP(1) = 0.
      CWKVP(2) = 32767.
      CWKVP(3) = 0.
      CWKVP(4) = 32767.
C
C  WSL clipping control parameters.
C
C  Clipping indicator is on (1=GCLIP)
C
      MRCLIP = GCLIP
C
C  Clipping rectangle.
C
      MRCREC(1) = MINXVD
      MRCREC(2) = MINYVD
      MRCREC(3) = MAXXVD
      MRCREC(4) = MAXYVD
C
      RETURN
      END
