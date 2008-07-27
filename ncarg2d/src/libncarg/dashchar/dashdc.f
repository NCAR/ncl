C
C	$Id: dashdc.f,v 1.7 2008-07-27 00:16:58 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DASHDC (IPAT,JCRT,JSIZE)
C
C Software dashed line package with character capability
C
C PURPOSE                DASHCHAR is a software dashed line package.
C
C USAGE                  First, either
C                             CALL DASHDB (IPAT)
C                        where IPAT is a 16-bit dash pattern as
C                        described in the subroutine DASHDB (see
C                        DASHLINE documentation), or
C                             CALL DASHDC (IPAT,JCRT,JSIZE)
C                        as described below.
C
C                        Then, call any of the following:
C                             CALL CURVED (X,Y,N)
C                             CALL FRSTD (X,Y)
C                             CALL VECTD (X,Y)
C                             CALL LASTD
C
C                        LASTD is called only after the last
C                        point of a line has been processed in VECTD.
C
C                        The following may also be called, but no
C                        smoothing will result:
C                             CALL LINED (XA,YA,XB,YB)
C
C
C ARGUMENTS              IPAT
C ON INPUT                 A character string of arbitrary length
C TO DASHDC                (60 characters seems to be a practical
C                          limit) which specifies the dash pattern
C                          to be used.  A dollar sign in IPAT
C                          indicates solid; an apostrophe indicates
C                          a gap; blanks are ignored.  Any character
C                          in IPAT which is not a dollar sign,
C                          apostrophe, or blank is considered to be
C                          part of a line label.  Each line label
C                          can be at most 15 characters in length.
C                          Sufficient white space is reserved in the
C                          dashed line for writing line labels.
C
C                        JCRT
C                          The length in plotter address units per
C                          $ or apostrophe.
C
C                        JSIZE
C                          Is the size of the plotted characters:
C                          . If between 0 and 3 , it is 1., 1.5, 2.
C                            and 3. times an 8 plotter address unit
C                            width.
C                          . If greater than 3, it is the character
C                            width in plotter address units.
C
C
C ARGUMENTS TO           CURVED(X,Y,N)
C OTHER LINE-DRAWING       X and Y are arrays of world coordinate values
C ROUTINES                 of length N or greater.  Line segments obeying
C                          the specified dash pattern are drawn to
C                          connect the N points.
C
C                        FRSTD(X,Y)
C                          The current pen position is set to
C                          the world coordinate value (X,Y)
C
C                        VECTD(X,Y)
C                          A line segment is drawn between the
C                          world coordinate value (X,Y) and the
C                          most recent pen position.  (X,Y) then
C                          becomes the most recent pen position.
C
C                        LINED(XA,XB,YA,YB)
C                          A line is drawn between world coordinate
C                          values (XA,YA) and (XB,YB).
C
C                        LASTD
C                          When using FRSTD and VECTD, LASTD may be
C                          called (no arguments needed). If the dashed
C                          line package was leaving a space for characters,
C                          and the line ended before there was enough space
C                          for the characters, the space left will be
C                          filled in if LASTD is called.
C
C
C ON OUTPUT                All arguments are unchanged for all routines.
C
C
C NOTE                     When switching from the regular plotting
C                          routines to a dashed line package the first
C                          call should not be to VECTD.
C
C ENTRY POINTS             DASHDB, DASHDC, CURVED, FRSTD, VECTD, LINED,
C                          RESET, LASTD, CFVLD, DRAWPV, DASHBD, DASHBDX
C
C COMMON BLOCKS            INTPR, DASHD1, DASHD2, DDFLAG, DCFLAG, DSAVE1,
C                          DSAVE6, DSAVE3, CFFLAG, DCFLAG
C
C REQUIRED LIBRARY         The ERPRT77 package and the SPPS.
C ROUTINES
C
C REQUIRED LEVEL OF GKS    0A
C
C I/O                      Plots solid or dashed lines, possibly with
C                          characters at intervals in the line.
C
C PRECISION                Single
C
C LANGUAGE                 FORTRAN
C
C HISTORY                  Written in 1969, standardized in November 1972.
C                          Made portable in September 1977 for use
C                          with all computer systems which
C                          support plotters with up to 15 bit resolution.
C                          Converted to FORTRAN77 and GKS in May 1984.
C
C ALGORITHM                The position in the dash pattern is
C                          remembered as points are processed.  The
C                          distance transversed in plotter address space
C                          is used to determine whether to draw segments,
C                          parts of segments, characters, or nothing.
C
C ACCURACY                 Plus or minus .5 plotter address units per call.
C                          There is no cumulative error.
C
C TIMING                   For solid or blank lines, there is almost no
C                          overhead.  Dashed lines take about 4 times as
C                          long as drawing solid lines.  (The line
C                          drawing software is so fast that the increase
C                          will not be noticed in most programs.) Patterns
C                          with characters do not take much longer than
C                          those without.
C
C PORTABILITY              FORTRAN77
C
C INTERNAL PARAMETERS      NAME   DEFAULT  FUNCTION  DECLARED IN BLOCK DATA
C                          ----   -------  --------
C
C                          IPAU      3     Number of plotter addresses per
C                                          element in the dash pattern for
C                                          solid lines and gaps. The
C                                          pattern is repeated every
C                                          IPAU*16 plotter address units.
C
C                          FPART     1.    Multiplicative factor for
C                                          first solid line segment.
C                                          This can be used to off-set
C                                          labels.  For example, if
C                                          FPART = .5, the first solid
C                                          line segment is only
C                                          one-half as long as the other
C                                          solid line segments.  This
C                                          moves all labels on this
C                                          line towards the beginning,
C                                          which reduces the
C                                          probability of the label
C                                          being written on top of a
C                                          label of a nearby line
C                                          drawn with FPART = 1.
C
C                          IGP       9     Flag to control whether a gap
C                                          is left for characters when
C                                          plotting.
C                                            = 9  Gap is left.
C                                            = 0  No gap is left.
C
C                          ICLOSE    6     An internal or external call to
C                                          set the pen (pen-up) to a
C                                          specific position is executed
C                                          only if this position is
C                                          more than ICLOSE metacode
C                                          address units away from the
C                                          current pen position (distance=
C                                          difference in X-coordinates +
C                                          difference in Y-coordinates).
C
C
C
      COMMON/INTPR/IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
     1    ICLOSE
C
C USER ENTRY POINT.
C DASHDC GIVES AN INTERNAL REPRESENTATION TO THE DASH PATTERN WHICH IS
C SPECIFIED IN ITS ARGUMENTS. THIS INTERNAL REPRESENTATION IS PASSED
C TO ROUTINE CFVLD IN THE COMMON-BLOCK DASHD1.
C
      CHARACTER*(*) IPAT
      CHARACTER*1   ICR
      CHARACTER*16  IPC(100)
C
C DASHD1 AND DASHD2 ARE USED
C FOR COMMUNICATION BETWEEN THE ROUTINES DASHDB, DASHDC, AND CFVLD.
C ISL, MNCSTR AND IGP ARE INITIALIZED IN DASHBDX.
C
      COMMON /DASHD1/  ISL,  L,  ISIZE,  IP(100),  NWDSM1,  IPFLAG(100)
     1                 ,MNCSTR, IGP
      COMMON /DASHD2/  IPC
C
C IFCFLG IS THE FIRST CALL FLAG FOR DASHDB AND DASHDC.
C IT IS INITIALIZED IN DASHBDX.
C
      COMMON /DDFLAG/ IFCFLG
C
C IFSTFL CONTROLS THAT FRSTD IS CALLED BEFORE VECTD IS CALLED (IN CFVLD)
C WHENEVER DASHDB OR DASHDC HAVE BEEN CALLED.
C IT IS INITIALIZED IN DASHBDX AND REFERENCED IN CFVLD.
C
      COMMON /DCFLAG/ IFSTFL
C
C LOCAL VARIABLES TO DASHDB AND DASHDC ARE SAVED IN DSAVE6 FOR
C THE NEXT CALL.
C
      COMMON /DSAVE6/ MASK, NCHRWD, NBWD, MNCST1
C
C Make definitions of a blank, a gap, and a solid externally visible.
C
      COMMON /BLGASO/ IBLK,IGAP,ISOL
      CHARACTER*1     IBLK,IGAP,ISOL
C
C SAVE ALL VARIABLES
C
      SAVE
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL DASHBD
C
C THE FOLLOWING CALL IS FOR LIBRARY STATISTICS GATHERING AT NCAR
C
      CALL Q8QST4 ('GRAPHX', 'DASHCHAR', 'DASHDC', 'VERSION  1')
C
C     NC IS THE NUMBER OF CHARACTERS IN IPAT
C
      NC = LEN(IPAT)
      IF (IFCFLG .EQ. 2) GOTO 10
C
C CHECK IF THE CONSTANTS IN THE BLOCKDATA DASHBDX ARE LOADED CORRECTLY
C
      IF (MNCSTR .EQ. 15) GOTO 6
      CALL SETER('DASHDC -- BLOCKDATA DASHBDX APPARENTLY NOT LOADED CORR
     1ECTLY',1,2)
    6 CONTINUE
C
C INITIALIZATION
C
      MNCST1 = MNCSTR + 1
C
C MASK IS AN ALL SOLID PATTERN TO BE PASSED TO OPTN (65535=177777B).
C
      MASK=IOR(ISHIFT(32767,1),1)
C
C
      IFCFLG = 2
C
C NCHRTS - NUMBER OF CHARS IN THIS HOLLERITH STRING.
C L      - NUMBER OF WORDS IN THE FINAL PATTERN, POINTER TO IP ARRAY.
C ISL    - FLAG FOR ALL SOLID PATTERN (1) OR ALL GAP PATTERN (-1).
C IFSTFL - FLAG TO CONTROL THAT FRSTD IS CALLED IN CFVLD BEFORE VECTD IS
C          CALLED, WHENEVER DASHDB OR DASHDC HAVE BEEN CALLED.
C
   10 CONTINUE
      NCHRTS = 0
      L = 0
      ISL = 0
      IFSTFL = 1
C
C RETRIEVE THE RESOLUTION AS SET BY THE USER.
C
      CALL GETUSV('XF',LXSAVE)
      CALL GETUSV('YF',LYSAVE)
C
C IADJUS - TO ADJUST NUMBERS TO THE GIVEN RESOLUTION.
C
      IADJUS = ISHIFT(1,15-LXSAVE)
      ICRT = JCRT*IADJUS
      ISIZE = JSIZE
      CHARW = REAL(ISIZE*IADJUS)
      IF (ISIZE .GT. 3) GO TO 30
      CHARW = 256. + REAL(ISIZE)*128.
      IF (ISIZE .EQ. 3) CHARW = 768.
C
   30 CONTINUE
      IF (ICRT .LT. 1) GO TO 230
      MODE = 2
C
C START MAIN LOOP
C
C THIS LOOP GENERATES THE IP ARRAY (NEEDED BY CURVED,VECTD,ETC.) FROM
C THE CHARACTER STRING IN IPAT.  EACH ITERATION OF THE LOOP PROCESSES
C ONE CHAR OF IPAT.  A SOLID OR GAP IS CONSIDERED TO BE A TYPE 1 ENTRY,
C AND A LABEL CHARACTER IS CONSIDERED TO BE A TYPE 2 ENTRY.
C
C IN THE CODE, L IS THE NUMBER OF CHANGES IN THE LINESTYLE (FROM GAP
C TO SOLID, SOLID TO CHARACTER, ETC.)  THE IP AND IPFLAG ARRAYS DESCRIBE
C THE LINE TO BE DRAWN, AND THESE ARRAYS ARE INDEXED FROM 1 TO L.  THE
C RELATIONSHIP BETWEEN IP AND IPFLAG IS:
C
C      IPFLAG(N)    IP(N)
C      ---------    -----
C          1        LENGTH (IN PLOTTER ADDRESS UNITS) OF SOLID LINE TO
C                   BE DRAWN.
C          0        NUMBER OF CHARACTERS TO BE PLOTTED.
C         -1        LENGTH (IN PLOTTER ADDRESS UNITS) OF GAP.
C
C THE 160 LOOP HANDLES 5 CASES:
C
C    1.)  CONTINUE TYPE 2 ENTRY (60-80)
C    2.)  START TYPE 2 ENTRY (80-90)
C    3.)  END TYPE 2 ENTRY AND START TYPE 1 ENTRY (90-160)
C    4.)  START TYPE 1 ENTRY, OR SWITCH TYPE 1 ENTRY FROM SOLID TO
C         GAP OR FROM GAP TO SOLID (140-160)
C    5.)  CONTINUE TYPE 1 ENTRY (150-160)
C
      DO 160 J=1,NC
C
C GET NEXT CHAR INTO ICR, RIGHT JUSTIFIED ZERO FILLED.
C
            ICR = IPAT(J:J)
C
C MODE SPECIFIES WHAT THE LAST CHARACTER PROCESSED WAS:
C
C    LAST ICR WAS $ (SOLID),      MODE IS 8
C    LAST ICR WAS ' (GAP),        MODE IS 2
C    LAST ICR WAS HOLLERITH CHAR, MODE IS 5
C
C NMODE SPECIFIES WHAT THE CURRENT CHARACTER TO BE PROCESSED IS:
C
C      ICR     NMODE
C      ---     -----
C       $        1
C       CHAR     0
C       '       -1
C
            NMODE = 0
            IF (ICR .EQ. IBLK) GO TO 160
            IF (ICR .EQ. IGAP) NMODE = -1
            IF (ICR .EQ. ISOL) NMODE = 1
            IF (L.EQ.0 .AND. NMODE.EQ.-1) MODE = 8
C
C NGO DETERMINES WHERE TO BRANCH BASED ON CASE TO BE PROCESSED.
C COMPUTE MODE FOR NEXT ITERATION.
C
            NGO = NMODE+MODE
            MODE = NMODE*3+5
            GO TO (150,80,140,90,60,90,140,80,150),NGO
C
C CHAR TO CHAR
C
C CASE 1) - CONTINUE TYPE 2 ENTRY.
C
   60      IF (NCHRTS .EQ. MNCSTR) GO TO 160
           NCHRTS = NCHRTS + 1
           IP(L) = NCHRTS
           IPC(L)(NCHRTS:NCHRTS) = ICR
           GO TO 160
C
C BLANK OR SOLID TO CHAR
C
C CASE 2) - START STRING ENTRY.  LGBSTR POINTS TO THE GAP WHICH
C           WILL CONTAIN THE STRING.
C
   80      LGBSTR = MIN(L+1,NP)
           L = MIN(LGBSTR+1,NP)
           IPFLAG(L) = 0
           NCHRTS    = 1
           IP(L)     = 1
           IPC(L)(NCHRTS:NCHRTS) = ICR
           GO TO 160
C
C CHAR TO SOLID OR GAP
C
C CASE 3) - END STRING ENTRY.  ICR IS A $ OR '.
C
   90      CONTINUE
           IP(LGBSTR) = CHARW*(REAL(NCHRTS) + .5)
           IPFLAG(LGBSTR) = -1
           IF (IGP .EQ. 0) IPFLAG(LGBSTR) = 1
C
C BLANK TO SOLID OR SOLID TO BLANK
C
C CASE 4) - START TYPE 1 ENTRY.
C
  140       L = MIN(L+1,NP)
            IP(L) = 0
C
C ADD TO A BLANK OR SOLID LINE
C
C CASE 5) - CONTINUE TYPE 1 ENTRY.  ICR IS A $ OR '.
C ADD ICRT UNITS TO THE PLOTTER ADDRESS UNITS IN IP(L).
C NMODE INDICATES IF IT IS A GAP OR A SOLID.
C
  150       IP(L) = IP(L) + ICRT
            IPFLAG(L) = NMODE
  160    CONTINUE
C
C IF LAST ICR PROCESSED WAS A LABEL CHARACTER, MUST END STRING
C ENTRY.
C
      IF (NGO.NE.2 .AND. NGO.NE.5 .AND. NGO.NE.8) GO TO 220
      IP(LGBSTR) = CHARW*(REAL(NCHRTS)+.5)
      IPFLAG(LGBSTR) = -1
      IF (IGP .EQ. 0) IPFLAG(LGBSTR) = 1
C
C IF IP ARRAY HAS ONLY ONE TYPE 1 ENTRY, SET ISL FLAG.
C
  220 IF (L .GT. 1) RETURN
      IBIG = ISHIFT(1,MAX(LXSAVE,LYSAVE))
      IF (IP(L) .GE. IBIG) GO TO 230
      IF (IPFLAG(L)) 240,240,230
  230 ISL = 1
      RETURN
  240 ISL = -1
      RETURN
      END
