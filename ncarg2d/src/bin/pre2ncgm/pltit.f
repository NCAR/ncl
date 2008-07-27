C
C	$Id: pltit.f,v 1.4 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PLTIT(NX,NY,PEN,IOS,STATUS)
C
C  CONTROL THE LINE DRAWING BUFFERS
C
C  INPUT
C       NX-X COORDINATE IN PLOTTER ADDRESS UNITS (0-32767)
C       NY-Y COORDINATE IN PLOTTER ADDRESS UNITS (0-32767)
C       PEN-PEN UP DOWN COMMAND
C               0-PEN UP (MOVE COMMAND)
C               1-PEN DOWN (DRAW COMMAND)
C  OUTPUT
C       IOS-IO STATUS FLAG VALID ONLY IF STATUS = I/O ERROR
C       STATUS-STATUS FLAG DEFINED BY THE COMMON TREROR
C
      INTEGER NX, NY, PEN, IOS, STATUS
C
      CALL PLOTIT(NX,NY,PEN)
C
      RETURN
      END
