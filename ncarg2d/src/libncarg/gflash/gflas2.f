C
C	$Id: gflas2.f,v 1.5 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GFLAS2
C
C_BEGIN PROLOGUE GFLAS2
C
C_PURPOSE
C    A call to this subroutine flags termination of output of
C    plotting instructions to Workstation Independent Segment
C    Storage as initiated by a previous call to GFLAS1.
C
C_DESCRIPTION
C    A call to this subroutine flags termination of output of
C    plotting instructions to Workstation Independent Segment
C    Storage (WISS) as initiated by a previous call to GFLAS1.
C
C_ARGUMENTS
C
C (None)
C
C_I/O
C    GFLAS2 terminates output to WISS.  If using the NCAR GKS
C    package, the output file which was opened with GFLAS1 is
C    closed.
C
C_ROUTINES CALLED
C    PLOTIT, SETER
C
C_COMMON BLOCKS
C    GFLASH
C
C_MAXIMUM GKS LEVEL
C    2A
C
C_LANGUAGE
C    FORTRAN
C
C_REVISION DATE
C    890117 (YYMMDD)
C
C_HISTORY
C    This subroutine was coded at NCAR by Fred Clare.  Its purpose
C    is to duplicate the functionality of the previous NCAR System
C    Plot Package entry FLASH2 in a GKS environment.
C
C_REFERENCES
C    "The System Plot Package", NCAR Technical Note number
C    NCAR-TN/162+IA, May, 1983.
C
C    "NCAR Graphics User's Guide", Version 2.00, NCAR Technical Note
C    number NCAR/TN-283+IA, August, 1987.
C
C_LONG DESCRIPTION
C    GFLAS2 deactivates WISS and re-activates all workstations
C    which were active before the call to GFLAS1 which initiated
C    the current GFLAS1/GFLAS2 sequence.  This results in all
C    subsequent plotting instructions being sent to all workstations
C    which were active prior to the current GFLAS1/GFLAS2 sequence.
C    If one is using the NCAR GKS package, the output file GFBNnn
C    is closed (where "nn" is the value of the argument to GFLAS1
C    in the current GFLAS1/GFLAS2 sequence.)
C
C    It is not legal to call FRAME (or clear workstations) between a
C    GFLAS1 call and a GFLAS2 call.
C
C_EXAMPLES
C    Example 1 --  Save a text string in a flash buffer (see GFLAS1
C                  for usage of that subroutine.)
C          SUBROUTINE SVTXT
C          CALL GFLAS1(9)
C          CALL GTX(.5,.5,'STRING')
C          CALL GFLAS2
C          RETURN
C          END
C
C_END PROLOGUE GFLAS2
C
C-------------------------------------------------------------------------
C
C  Common for FLASH package.
C
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C     CHARACTER*80 IDR(1),ODR(1)
C
      SAVE
C
C  Check on proper mode value.
C
      IF (MODEF .NE. 1) GO TO 101
      MODEF = 2
C
C  Flush the PLOTIT buffer.
C
      CALL PLOTIT(0,0,0)
C
      CALL GCLSG
C
C  Restore workstation states to what they were before call to
C  GFLAS1.  If WISS was not active, deactivate it since GFLAS1
C  made it active; reactivate all other previously active
C  workstations.
C
C
C  Determine the index of WISS.
C
      DO 20 I=1,NUMOP
      IF (IOPWKS(I) .EQ. IWISSI) THEN
        NWISS = I
        GO TO 30
      ENDIF
   20 CONTINUE
      GO TO 102
   30 CONTINUE
C
C  Deactive WISS if it was not active before call to GFLAS1.
C
      IF (IOACT(NWISS) .EQ. 0) CALL GDAWK(IWISSI)
C
C  Handle non-WISS workstations.
C
      DO 10 I=1,NUMOP
      IF (IOPWKS(I).NE.IWISSI .AND. IOACT(I).EQ.1) CALL GACWK(IOPWKS(I))
   10 CONTINUE
C
C  Inform NCAR GKS that we are finished in GFLAS2
C
C     IDR(1) = '0'
C     IFID = -1394
C     CALL PLOTIT(0,0,2)
C     CALL GESC(IFID,1,IDR,1,IDUM,ODR)
C
      RETURN
C
  101 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS2 -- GFLAS2 CALLED WITHOUT CALLING GFLAS1',6,2)
      STOP
  102 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS2 -- WISS NOT OPEN AT GFLAS2 TIME',8,2)
      STOP
C
      END
