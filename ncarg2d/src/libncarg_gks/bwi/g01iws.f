C
C	$Id: g01iws.f,v 1.4 2000-08-22 15:09:34 haley Exp $
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
      SUBROUTINE G01IWS
C
C  Initialization of constants.
C
C  Initialize workstation state list and other internal constants
C  as required upon open workstation.
C
      include 'g01prm.h'
      include 'g01wsl.h'
      include 'g01ins.h'
      include 'gksenu.h'
C
C  Internal constants.
C
C  MIN and MAX VDC address limits in x and y directions, the VDC extent.
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
C  Color table change, background color change.
C
      MCTCHG = GNO
      MBCCHG = GNO
C
C  WSL items.
C
C  State is inactive (0=GINACT)
C
      MSTATE = GINACT
C
C  Deferral mode is ASTI (3=GASTI)
C
      MDEFMO = 3
C
C  Implicit regeneration mode is allowed (1=GALLOW)
C
      MREGMO = 1
C
C  Display surface is empty (1=GEMPTY)
C
      MDEMPT = GEMPTY
C
C  New frame necessary at update is NO (0=GNO)
C
      MNFRAM = GNO
C
C  Workstation transformation update state is NOT PENDING (0=GNPEND)
C
      MTUS = GNPEND
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
C  Requested workstation viewport is real metafile address space.
C
      RWKVP(1) = 0.
      RWKVP(2) = 32767.
      RWKVP(3) = 0.
      RWKVP(4) = 32767.
C
C  Current workstation viewport is metafile address space.
C
      CWKVP(1) = 0.
      CWKVP(2) = 32767.
      CWKVP(3) = 0.
      CWKVP(4) = 32767.
C
C  Color indices are sorted by default.
C
      MCSORT = 1
C
C  Number of currently defined color indices is 0.
C
      MOL = 0
C
C  Overflow flag for color index arrays is NO (0=GNO).
C
      MCOVFL = GNO
C
C  WSL clipping control parameters.
C
C  Clipping indicator is ON (1=GCLIP)
C
      MRCLIP = GCLIP
C
C  Clipping rectangle is metafile address space, corner-point form.
C
      MRCREC(1) = MINXVD
      MRCREC(2) = MINYVD
      MRCREC(3) = MAXXVD
      MRCREC(4) = MAXYVD
C
      RETURN
      END
