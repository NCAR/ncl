C
C	$Id: encd.f,v 1.5 2008-07-27 00:17:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ENCD (VALU,ASH,IOUT,NC,IOFFD)
C
C ON INPUT     VALU      Floating point number from which a label is
C                        to be created in IOUT.
C
C              ASH       A scale factor used when IOFFD is non-zero.
C                        ASH should be non-negative.
C
C              IOFFD     If IOFFD .EQ. 0
C                             a label which reflects the magnitude
C                             of VALU is created.  Legal values for
C                             VALU are  0.0  and any value such that
C                             .1 .LE. ABS(VALU) .LE. 99999.49999...
C                             The label created in IOUT will have 3
C                             to 6 characters depending on the value
C                             of VALU.  See IOUT below.
C                        IF IOFFD .NE. 0
C                             a label is created which reflects the
C                             value in  VALU  scaled by  ASH.
C                             Legal values for VALU are 0. or
C                             any number such that
C                             1. .LE. ASH*ABS(VALU) .LT. 1000.
C                             The label created in IOUT will have
C                             1 to 3 characters, depending on the
C                             magnitude of ASH*VALU.  See IOUT below.
C
C ON OUTPUT    IOUT      Contains the created label.  It should have no
C                        leading blanks.  See NC.
C
C              NC        The number of characters in the label created
C                        in  IOUT.  NC will be between 1 and 6 inclusive.
C
      SAVE
      CHARACTER*(*)   IOUT
      CHARACTER*7     IFMT
C
      VAL = VALU
      IF (IOFFD .NE. 0) GO TO 103
      IF (VAL) 101,104,101
  101 CONTINUE
      LOG = INT((ALOG10(ABS(VAL))+.00001)+5000.)-5000
      V = VAL
      NS = MAX(4,MIN(6,LOG+2))
      ND = MIN(3,MAX(0,2-LOG))
      IF (VAL.LT.0)  NS = NS + 1
  102 CONTINUE
      WRITE (IFMT,'(A2,I2,A1,I1,A1)') '(F',NS,'.',ND,')'
      WRITE (IOUT,IFMT) V
      NC = NS
      IF (LOG.GE.3)  NC = NC - 1
      RETURN
  103 CONTINUE
      NS = 4
      IF (VAL.LT.0.)  NS=5
      IF (VAL.EQ.0.)  NS=2
      ND = 0
      V = VAL*ASH
      LOG = 100
      GO TO 102
  104 CONTINUE
      NS = 3
      ND = 1
      LOG = -100
      V = 0.
      GO TO 102
C
      END
