C
C	$Id: gflas1.f,v 1.7 2008-07-27 00:17:13 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GFLAS1(INAME)
C
C_BEGIN PROLOGUE GFLAS1
C
C_PURPOSE
C    A call to this subroutine flags that all subsequently generated
C    plotting instructions should be saved only in Workstation
C    Independent Segment Storage until a call to GFLAS2 is made.
C
C_DESCRIPTION
C    A call to this subroutine flags that all subsequently generated
C    plotting instructions should be saved only in Workstation
C    Independent Segment Storage (WISS) until a call to GFLAS2 is made.
C    Consult the documentation to GFLAS2 for its usage.
C
C    WISS must be open before calling GFLAS1.  If using NCAR GKS, a
C    call to GOPWK with final argument 3 will suffice for this; if
C    using a non-NCAR GKS, the final argument to GOPWK must be the
C    workstation type of WISS.  Consult GKS documentation for details
C    on the arguments to GOPWK.
C
C_ARGUMENTS
C
C INAME   (INPUT,SINGLE,VARIABLE)  An integer variable in the range
C         [0,99] which is used as a user-defined identifier for the
C         dataset of graphics instructions to be saved between the
C         GFLAS1 call and a subsequent GFLAS2 call.
C
C_I/O
C    GFLAS1 initiates output to WISS.  If using the NCAR GKS
C    package, the output disk file is opened.
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
C    Plot Package entry FLASH1 in a GKS environment.
C
C_REFERENCES
C    "The System Plot Package", NCAR Technical Note number
C    NCAR-TN/162+IA, May, 1983.
C
C    "NCAR Graphics User's Guide", Version 2.00, NCAR Technical Note
C    number NCAR/TN-283+IA, August, 1987.
C
C_LONG DESCRIPTION
C    WISS must be open before calling GFLAS1.  GFLAS1 closes all
C    currently active workstations except WISS, and activates WISS.
C    This results in all subsequent plotting instructions being sent
C    only to WISS.  If one is using the NCAR GKS package, WISS is
C    implemented on the disk of the host machine, and plotting
C    instructions are saved on disk.  By default, the flash buffers
C    (or segments in GKS parlance) are named as "GSEGuuuupppp_nn" 
C    where "uuuu" is the user id, and "pppp" is the process id, 
C    and "nn" is the buffer ID provided in the argument INAME 
C    (described above).  These are put in a temporary directory and
C    automatically deleted when WISS is closed.  There is no way
C    to save these files.  The only way to save flash buffers is
C    to name them, using ESCAPE -1389 to supply a root name.  These
C    files will be written to the local current directory.  All files 
C    with names beginning with "GSEG" will be deleted when WISS is 
C    closed; all other files will not be deleted.  To save just a 
C    single flash buffer, set the root name to something different 
C    than "GSEG" and then write that segment, then change the root 
C    back to "GSEG".
C
C    It is not legal to call FRAME (or clear workstations) between a
C    GFLAS1 call and a GFLAS2 call.
C
C_EXAMPLES
C    Example 1 --  Save a text string in a flash buffer (see GFLAS2
C                  for usage of that subroutine.)
C          SUBROUTINE SVTXT
C          CALL GFLAS1(9)
C          CALL GTX(.5,.5,'STRING')
C          CALL GFLAS2
C          RETURN
C          END
C
C_END PROLOGUE GFLAS1
C
C-------------------------------------------------------------------------
C
C  IOPWKS is an array containing the workstation identifiers for
C  all open workstations; IOACT is an array of flags indicating
C  whether the open workstation is also active.
C
      COMMON /GFLASH/MODEF,IOPWKS(100),IOACT(100),NUMOP,IWISSI
C
C     CHARACTER*80 IDR(1),ODR(1)
C
      SAVE
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL GFLSBD
C
C  Check on proper mode value (MODEF keeps track of where we are
C  in a GFLASH sequence.)
C
      IF (MODEF .EQ. 1) GO TO 101
      MODEF = 1
C
C  Inform NCAR GKS that we are in GFLAS1
C
C     IDR(1) = '1'
C     IFID = -1394
C     CALL PLOTIT(0,0,2)
C     CALL GESC(IFID,1,IDR,1,IDUM,ODR)
C     CALL PLOTIT(0,0,2)
C
C  Check on the range of the input argument.
C
      IBNAME = INAME
      IF (IBNAME.LT.0 .OR. IBNAME.GT.99) GO TO 108
C
C  Check if GKS is open.
C
      CALL GQOPS(IOPSTA)
      IF (IOPSTA .LT. 1) GO TO 109
C
C  Make sure that this GKS implementation has WISS (level 2).
C
      CALL GQEWK(1,IER,NUMB,NTYP)
      DO 10 I=1,NUMB
        CALL GQEWK(I,IER,NUMB,NTYP)
        CALL GQWKCA(NTYP,IER,NCAT)
        IF (NCAT .EQ. 3) GO TO 20
   10 CONTINUE
      GO TO 102
   20 CONTINUE
C
C  Make sure that WISS is open (IWISSI is initialized to an
C  arbitrary illegal value).
C
      IWISSI = -1335
      CALL GQOPWK(1,IER,IOL,IWKID)
      NUMOP = IOL
      IF (NUMOP .LT. 1) GO TO 103
      IF (NUMOP .GT. 100) GO TO 104
      DO 30 I=1,NUMOP
        CALL GQOPWK(I,IER,IOL,IOPWKS(I))
        IOACT(I) = 0
        CALL GQWKS(IOPWKS(I),IER,IOACT(I))
        CALL GQWKC(IOPWKS(I),IER,ICON,ITYPE)
        CALL GQWKCA(ITYPE,IER,ICAT)
        IF (ICAT .EQ. 3) IWISSI = IOPWKS(I)
   30 CONTINUE
      IF (IWISSI .EQ. -1335) GO TO 103
C
C  De-activate all active workstations.
C
      DO 40 I=1,NUMOP
      IF (IOACT(I) .EQ. 1) THEN
        CALL GDAWK(IOPWKS(I))
      ENDIF
   40 CONTINUE
C
C  Activate WISS.
C
      CALL GACWK(IWISSI)
C
C  Flush PLOTIT buffer.
C
      CALL PLOTIT(0,0,2)
C
C  If segment name is currently in use, delete it.
C
      CALL GQSGUS(1,IER,IOL,ISGNA)
      IF (IOL .GT. 0) THEN
        DO 50 I=1,IOL
        CALL GQSGUS(I,IER,IOL,ISGNA)
        IF (ISGNA .EQ. INAME) THEN
          CALL GDSG(INAME)
          GO TO 60
        ENDIF
   50   CONTINUE
      ENDIF
   60 CONTINUE
C
C  Create segment.
C
      CALL GCRSG(INAME)
      RETURN
C
  101 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS1 -- GFLAS1 CALLED CONSECUTIVELY WITHOUT AN INTE
     -RVENING GFLSH2 CALL',1,2)
      STOP
  102 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS1 -- THE NON NCAR GKS PACKAGE IS NOT LEVEL 2',
     -           2,2)
      STOP
  103 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS1 -- WISS MUST BE OPEN BEFORE CALL TO GFLAS1',
     -           3,2)
      STOP
  104 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS1 -- MAXIMUM NUMBER OF OPEN WORKSTATIONS ALLOWED
     - IS 100',4,2)
      STOP
  108 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS1 -- BUFFER IDENTIFIER .LT. 0, OR .GT. 99',8,2)
      STOP
  109 CONTINUE
      CALL GECLKS
      CALL SETER(' GFLAS1 -- GKS MUST BE OPEN BEFORE A CALL TO GFLAS1',
     -  9,2)
      STOP
C
      END
