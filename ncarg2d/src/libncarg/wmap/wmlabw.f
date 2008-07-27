C
C	$Id: wmlabw.f,v 1.6 2008-07-27 00:17:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMLABW(X,Y,LABEL)
C
C  Plot the label contained in the character variable LABEL 
C  centered at (X,Y), where X and Y are world coordinates.  
C  The labels are drawn with a shadow box.
C
      include 'wmcomn.h'
C
      CHARACTER*(*) LABEL
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQFAIS(IER,IFAISO)
      CALL GQFACI(IER,IFCLRO)
      CALL GQPLCI(IER,ILCLRO)
      CALL GSFACI(ICOLOR)
      CALL GSPLCI(ICOLOR)
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
      CALL GQCNTN(IER,NTRO)
      CALL GSELNT(0)
      CALL PCSETI('TE',1)
C
      SIZEL = WSIZEW
C
C  Save Plotchar parameters.
C
      CALL PCGETI ('BF - BOX FLAG',IBFO)
      CALL PCGETR ('BM - BOX MARGIN',BMO)
      CALL PCGETR ('BX - BOX SHADOW X OFFSET',BXO)
      CALL PCGETR ('BY - BOX SHADOW Y OFFSET',BYO)
      CALL PCGETI ('BC(1) - BOX COLOR - BOX OUTLINE    ',IBC1)
      CALL PCGETI ('BC(2) - BOX COLOR - BOX FILL       ',IBC2)
      CALL PCGETI ('BC(3) - BOX COLOR - BOX SHADOW FILL',IBC3)
      CALL PCGETI ('CC - CHARACTER COLOR',ICCO)
      CALL PCGETI ('OC - outline color',IOC)
      CALL PCGETI ('OF - outline flag',IFC)
      CALL PCGETI ('FN - font number',IFNO)
C
C  Draw a label with a box around it.
C
      CALL PCSETI ('BF - BOX FLAG',7)
      CALL PCSETR ('BM - BOX MARGIN',.35)
      CALL PCSETR ('BX - BOX SHADOW X OFFSET',-0.20)
      CALL PCSETR ('BY - BOX SHADOW Y OFFSET',-0.20)
      CALL PCSETI ('BC(1) - BOX COLOR - BOX OUTLINE    ',IRGLC1)
      CALL PCSETI ('BC(2) - BOX COLOR - BOX FILL       ',IRGLC2)
      CALL PCSETI ('BC(3) - BOX COLOR - BOX SHADOW FILL',IRGLC3)
      CALL PCSETI ('CC - CHARACTER COLOR',IRGLC4)
      CALL PCSETI ('FN',22)
      IF (IRGLC5 .GE. 0) THEN
        CALL PCSETI ('OF - outline flag',1)
        CALL PCSETI ('OC - outline color',IRGLC5)
      ENDIF
      CALL PLCHHQ (XNDC,YNDC,LABEL,SIZEL,0.,0.)
C
C  Restore Plotchar parameters.
C
      CALL PCSETI ('BF - BOX FLAG',IBFO)
      CALL PCSETR ('BM - BOX MARGIN',BMO)
      CALL PCSETR ('BX - BOX SHADOW X OFFSET',BXO)
      CALL PCSETR ('BY - BOX SHADOW Y OFFSET',BYO)
      CALL PCSETI ('BC(1) - BOX COLOR - BOX OUTLINE    ',IBC1)
      CALL PCSETI ('BC(2) - BOX COLOR - BOX FILL       ',IBC2)
      CALL PCSETI ('BC(3) - BOX COLOR - BOX SHADOW FILL',IBC3)
      CALL PCSETI ('CC - CHARACTER COLOR',ICCO)
      CALL PCSETI ('OC - outline color',IOC)
      CALL PCSETI ('OF - outline flag',IFC)
      CALL PCSETI ('FN - font number',IFNO)
C
C  Restore GKS environment.
C
      CALL GSFAIS(IFAISO)
      CALL GSFACI(IFCLRO)
      CALL GSPLCI(ILCLRO)
      CALL GSELNT(NTRO)
      CALL PCSETI('TE',0)
C
      RETURN
      END
