
      PROGRAM AREX02
C
C This program illustrates the use of the routine ARMVAM, particularly
C as it is used in the process of recovering from area map overflow
C problems.  Because this example is intended to be strictly FORTRAN
C 77, no attempt is made to do real dynamic storage allocation;
C instead, an area map array of a fixed size is used, and the AREAS
C routines are only told about a part of that array.  Still, the idea
C is much the same as one would use in C or, presumably, in FORTRAN
C 90 (if and when compilers become generally available for FORTRAN 90).
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define the size of the area map array to be used.  The part of this
C used by the AREAS routines will fluctuate as needed.
C
        PARAMETER (LAMA=6000)
C
C Define the sizes of the work arrays to be used by ARSCAM.
C
        PARAMETER (NCRA=1000,NGPS=10)
C
C Declare the area map array.
C
        DIMENSION IAMA(LAMA)
C
C Declare the work arrays to be used by ARSCAM.
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C Declare arrays to hold a perimeter, to be used for all three groups
C of edges.
C
        DIMENSION PERIX(5),PERIY(5)
C
C Declare arrays for group 1 edges.
C
        DIMENSION G1E1X(9),G1E1Y(9)
        DIMENSION G1E2X(5),G1E2Y(5)
C
C Declare arrays for group 3 edges.
C
        DIMENSION G3E1X(2),G3E1Y(2)
        DIMENSION G3E2X(2),G3E2Y(2)
        DIMENSION G3E3X(2),G3E3Y(2)
        DIMENSION G3E4X(2),G3E4Y(2)
        DIMENSION G3E5X(2),G3E5Y(2)
        DIMENSION G3E6X(2),G3E6Y(2)
        DIMENSION G3E7X(2),G3E7Y(2)
        DIMENSION G3E8X(2),G3E8Y(2)
        DIMENSION G3E9X(2),G3E9Y(2)
C
C Declare arrays for group 5 edges.
C
        DIMENSION G5E1X(7),G5E1Y(7)
C
C Declare the routine that will color the areas defined by the area map.
C
        EXTERNAL COLRAM
C
C Define the perimeter for all three groups of edges.
C
        DATA PERIX / .90,.90,.10,.10,.90 /
        DATA PERIY / .10,.90,.90,.10,.10 /
C
C Define the group 1 edges.
C
        DATA G1E1X / .75,.68,.50,.32,.25,.32,.50,.68,.75 /
        DATA G1E1Y / .50,.68,.75,.68,.50,.32,.25,.32,.50 /
C
        DATA G1E2X / .60,.60,.40,.40,.60 /
        DATA G1E2Y / .40,.60,.60,.40,.40 /
C
C Define the group 3 edges.
C
        DATA G3E1X / .10,.20 /
        DATA G3E1Y / .80,.90 /
C
        DATA G3E2X / .10,.40 /
        DATA G3E2Y / .60,.90 /
C
        DATA G3E3X / .10,.60 /
        DATA G3E3Y / .40,.90 /
C
        DATA G3E4X / .10,.80 /
        DATA G3E4Y / .20,.90 /
C
        DATA G3E5X / .20,.90 /
        DATA G3E5Y / .10,.80 /
C
        DATA G3E6X / .40,.90 /
        DATA G3E6Y / .10,.60 /
C
        DATA G3E7X / .60,.90 /
        DATA G3E7Y / .10,.40 /
C
        DATA G3E8X / .80,.90 /
        DATA G3E8Y / .10,.20 /
C
        DATA G3E9X / .40,.20 /
        DATA G3E9Y / .70,.50 /
C
C Define the group 5 edges.
C
        DATA G5E1X / .50,.80,.80,.50,.50,.20,.35 /
        DATA G5E1Y / .50,.50,.80,.80,.50,.20,.35 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Change the GKS "fill area interior style" to be "solid".
C
        CALL GSFAIS (1)
C
C Define the colors to be used.
C
        CALL GSCR (IWKID,0,0.,0.,0.)
        CALL GSCR (IWKID,1,1.,1.,1.)
C
        DO 101 I=1,9
          S=MAX(0.,MIN(1.,REAL(I)/9.))
          T=MAX(0.,MIN(1.,1.-S))
          CALL GSCR (IWKID,10+I,S,T,0.)
          CALL GSCR (IWKID,20+I,S,T,1.)
  101   CONTINUE
C
C Define the mapping from the user system to the plotter frame.
C
        CALL SET (.05,.95,.05,.95,0.,1.,0.,1.,1)
C
C Tell AREAS to make identifiers larger and further from the arrows
C and make the arrowheads bigger.
C
        CALL ARSETR ('ID - IDENTIFIER DISTANCE',.02)
        CALL ARSETR ('IS - IDENTIFIER SIZE',.01)
        CALL ARSETR ('AL - ARROWHEAD LENGTH',.04)
        CALL ARSETR ('AW - ARROWHEAD WIDTH',.008)
C
C Initialize the variable that keeps track of how much space is
C currently used in the area map.
C
        NAMA=500
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,NAMA)
C
C Put group 1 edges into the area map.  Instead of calling the AREAS
C routine AREDAM directly, we call an example routine that puts the
C package SETER into recovery mode, checks for overflow of the area
C map array, and uses ARMVAM to recover.
C
        CALL EXEDAM (IAMA,LAMA,NAMA,PERIX,PERIY,5,1, 0,-1)
        CALL EXEDAM (IAMA,LAMA,NAMA,G1E1X,G1E1Y,9,1, 2, 1)
        CALL EXEDAM (IAMA,LAMA,NAMA,G1E2X,G1E2Y,5,1, 1, 2)
C
C Put group 3 edges into the area map.  Again, instead of calling the
C AREAS routine AREDAM directly, we call EXEDAM to allow for error
C recovery.
C
        CALL EXEDAM (IAMA,LAMA,NAMA,PERIX,PERIY,5,3, 0,-1)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E1X,G3E1Y,2,3, 1, 2)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E2X,G3E2Y,2,3, 2, 3)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E3X,G3E3Y,2,3, 3, 4)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E4X,G3E4Y,2,3, 4, 5)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E5X,G3E5Y,2,3, 5, 6)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E6X,G3E6Y,2,3, 6, 7)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E7X,G3E7Y,2,3, 7, 8)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E8X,G3E8Y,2,3, 8, 9)
        CALL EXEDAM (IAMA,LAMA,NAMA,G3E9X,G3E9Y,2,3, 9,10)
C
C Put group 5 edges into the area map.  Again, instead of calling the
C AREAS routine AREDAM directly, we call EXEDAM to allow for error
C recovery.
C
        CALL EXEDAM (IAMA,LAMA,NAMA,PERIX,PERIY,5,5, 0,-1)
        CALL EXEDAM (IAMA,LAMA,NAMA,G5E1X,G5E1Y,7,5,-1, 0)
C
C Preprocess the area map.  Again, instead of calling the AREAS
C routine ARPRAM directly, we call EXPRAM to allow for error
C recovery.  Do debug plots to make sure things are working okay.
C
        CALL ARDBPA (IAMA,1,'BEFORE CALLING ARPRAM - GROUP 1')
        CALL ARDBPA (IAMA,3,'BEFORE CALLING ARPRAM - GROUP 3')
        CALL ARDBPA (IAMA,5,'BEFORE CALLING ARPRAM - GROUP 5')
        CALL EXPRAM (IAMA,LAMA,NAMA,0,0,0)
        CALL ARDBPA (IAMA,1,'AFTER CALLING ARPRAM - GROUP 1')
        CALL ARDBPA (IAMA,3,'AFTER CALLING ARPRAM - GROUP 3')
        CALL ARDBPA (IAMA,5,'AFTER CALLING ARPRAM - GROUP 5')
C
C Pack the contents of the area map into the smallest possible space.
C
        CALL ARMVAM (IAMA,IAMA,IAMA(1)-(IAMA(6)-IAMA(5)-1))
C
C Scan the area map.  Again, instead of calling the AREAS routine
C ARSCAM directly, we call EXSCAM to allow for error recovery.
C
        CALL EXSCAM (IAMA,LAMA,NAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,
     +                                                     COLRAM)
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END



      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
C This routine colors the areas defined by the area map.
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C Pick off the individual group identifiers.
C
        IAI1=0
        IAI3=0
        IAI5=0
C
        DO 101 I=1,NGPS
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
          IF (IAGI(I).EQ.3) IAI3=IAAI(I)
          IF (IAGI(I).EQ.5) IAI5=IAAI(I)
  101   CONTINUE
C
C Skip coloring if either of the first two area identifiers is zero or
C negative or if the final one is negative.
C
        IF (IAI1.LE.0.OR.IAI3.LE.0.OR.IAI5.LT.0) GO TO 102
C
C Otherwise, color the area, using a color index which is obtained by
C combining the area identifiers for groups 1 and 3.
C
        CALL GSFACI (10*IAI1+IAI3)
        CALL GFA    (NCRA-1,XCRA,YCRA)
C
C Done.
C
  102   RETURN
C
      END



      SUBROUTINE EXEDAM (IAMA,LAMA,NAMA,XCRA,YCRA,NCRA,IGID,IAIL,IAIR)
C
        DIMENSION IAMA(LAMA),XCRA(NCRA),YCRA(NCRA)
C
C This routine implements an interface to the AREAS routine AREDAM
C that recovers from area map overflow problems.
C
        CHARACTER*113 SEMESS
C
C Put SETER into recovery mode, saving the previous setting of the
C recovery-mode flag in IROLD.
C
        CALL ENTSR (IROLD,1)
C
C Attempt to put the edges in the error map.
C
  101   CALL AREDAM (IAMA,XCRA,YCRA,NCRA,IGID,IAIL,IAIR)
C
C See if a recoverable error occurred during the call to AREDAM.
C
        IF (NERRO(NERR).NE.0) THEN
C
C A recoverable error occurred.  See if it was due to overflowing the
C area map array and if we can do something about it.
C
          IF (SEMESS(2).EQ.'AREA-MAP ARRAY OVERFLOW'.AND.
     +                                          NAMA.LT.LAMA) THEN
C
C Recover from an area map array overflow.  First, log what's happening.
C
            PRINT * , 'EXEDAM - OVERFLOW RECOVERY - NAMA = ', NAMA
C
C Clear the internal error flag in SETER.
C
            CALL ERROF
C
C Move the area map to a slightly larger part of the area map array.
C
            NAMA=MIN(NAMA+100,LAMA)
            CALL ARMVAM (IAMA,IAMA,NAMA)
C
C Go back to try the call to AREDAM again.
C
            GO TO 101
C
          ELSE
C
C Either the error is not an overflow error or we can't do anything
C about it.  Exit with a fatal error message.
C
            CALL SETER ('EXEDAM - CAN''T GET AROUND AREDAM ERROR',1,2)
C
          END IF
C
        ELSE
C
C No recoverable error occurred.  Restore the original value of SETER's
C recovery-mode flag.
C
          CALL RETSR (IROLD)
C
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE EXPRAM (IAMA,LAMA,NAMA,IFL1,IFL2,IFL3)
C
        DIMENSION IAMA(LAMA)
C
C This routine implements an interface to the AREAS routine ARPRAM
C that recovers from area-map overflow problems.
C
        CHARACTER*113 SEMESS
C
C Put SETER into recovery mode, saving the previous setting of the
C recovery-mode flag in IROLD.
C
        CALL ENTSR (IROLD,1)
C
C Attempt to pre-process the area map.
C
  101   CALL ARPRAM (IAMA,IFL1,IFL2,IFL3)
C
C See if a recoverable error occurred during the call to ARPRAM.
C
        IF (NERRO(NERR).NE.0) THEN
C
C A recoverable error occurred.  See if it was due to overflowing the
C area map array and if we can do something about it.
C
          IF (SEMESS(2).EQ.'AREA-MAP ARRAY OVERFLOW'.AND.
     +                                                NAMA.LT.LAMA) THEN
C
C Recover from an area map array overflow.  First, log what's happening.
C
            PRINT * , 'EXPRAM - OVERFLOW RECOVERY - NAMA = ', NAMA
C
C Clear the internal error flag in SETER.
C
            CALL ERROF
C
C Move the area map to a slightly larger part of the area map array.
C
            NAMA=MIN(NAMA+100,LAMA)
            CALL ARMVAM (IAMA,IAMA,NAMA)
C
C Go back to try the call to ARPRAM again.
C
            GO TO 101
C
          ELSE
C
C Either the error is not an overflow error or we can't do anything
C about it.  Exit with a fatal error message.
C
            CALL SETER ('EXPRAM - CAN''T GET AROUND ARPRAM ERROR',1,2)
C
          END IF
C
        ELSE
C
C No recoverable error occurred.  Restore the original value of SETER's
C recovery-mode flag.
C
          CALL RETSR (IROLD)
C
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE EXSCAM (IAMA,LAMA,NAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,
     +                                                           URPA)
C
        DIMENSION IAMA(LAMA),XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C This routine implements an interface to the AREAS routine ARPRAM
C that recovers from area-map overflow problems.
C
        CHARACTER*113 SEMESS
C
C Put SETER into recovery mode, saving the previous setting of the
C recovery-mode flag in IROLD.
C
        CALL ENTSR (IROLD,1)
C
C Attempt to scan the area map.
C
  101   CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,URPA)
C
C See if a recoverable error occurred during the call to ARSCAM.
C
        IF (NERRO(NERR).NE.0) THEN
C
C A recoverable error occurred.  See if it was due to overflowing the
C area map array and if we can do something about it.
C
          IF (SEMESS(2).EQ.'AREA-MAP ARRAY OVERFLOW'.AND.
     +                                                NAMA.LT.LAMA) THEN
C
C Recover from an area map array overflow.  First, log what's happening.
C
            PRINT * , 'EXSCAM - OVERFLOW RECOVERY - NAMA = ', NAMA
C
C Clear the internal error flag in SETER.
C
            CALL ERROF
C
C Move the area map to a slightly larger part of the area map array.
C
            NAMA=MIN(NAMA+100,LAMA)
            CALL ARMVAM (IAMA,IAMA,NAMA)
C
C Go back to try the call to ARPRAM again.
C
            GO TO 101
C
          ELSE
C
C Either the error is not an overflow error or we can't do anything
C about it.  Exit with a fatal error message.
C
            CALL SETER ('EXSCAM - CAN''T GET AROUND ARSCAM ERROR',1,2)
C
          END IF
C
        ELSE
C
C No recoverable error occurred.  Restore the original value of SETER's
C recovery-mode flag.
C
          CALL RETSR (IROLD)
C
        END IF
C
C Done.
C
        RETURN
C
      END
