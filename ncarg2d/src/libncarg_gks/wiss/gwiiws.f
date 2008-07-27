C
C	$Id: gwiiws.f,v 1.6 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
