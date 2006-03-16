
      PROGRAM THSTMV
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL HSTMV(IERR)
C
C Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
      SUBROUTINE HSTMV (IERROR)
C
C PURPOSE                To provide a demonstration of the Histogram
C                        utility when special flagged values occur in
C                        the input data.
C
C USAGE                  CALL HSTMV (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An error flag
C                          = 0, If the test is successful,
C                          = 1, otherwise.
C
C I/O                    If the test is successful, the message
C
C                          HISTGR TEST SUCCESSFUL  . . .  SEE PLOT
C                          TO VERIFY PERFORMANCE
C
C                        is written on unit 6.
C
C                        Verify success by examing the plot produced.
C
C PRECISION              Single
C
C REQUIRED LIBRARY       HISTGR
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN 77
C
C HISTORY                Written by Bob Lackman, October 1993.
C
C ALGORITHM              THSTMV computes data and calls HISTGR
C                        with special values
C
C
C  Array DAT1 is filled with values to be used as input for HISTGR
C
      PARAMETER ( NDIM=320, NCLS=17, NWRK=374 )
C
C  NWRK = NDIM + 3*(NCLS+1)
C
      REAL  DAT1(NDIM,2), X, WORK(NWRK)
      REAL  CLASS(NCLS+1)
      REAL  ARRAY(2)
      CHARACTER*1 IFC
C
C Change the Plotchar special character code from a : to a @
C
      IFC(1:1) = '@'
      CALL PCSETC('FC',IFC)
C
C Change the print font to the high quality filled times roman.
C
      CALL PCSETC('FN','times-roman')
C
C     Plot 1:   IFLAG = 0, input values are sorted into 11 equal classes
C
      IFLAG = 0
      NCLASS = 11
      NPTS = 320
C
      DO 105 I=1,NPTS
        DO 105 J=1,2
          DAT1(I,J)=0.
  105 CONTINUE
C
      DO  110 I=1,NPTS
        X = REAL(I)
        DAT1(I,1) = 10. * ALOG10(0.1*X+1.)
  110 CONTINUE
C
C Put in some special values
C
	DAT1(7,1)  = -999.
	DAT1(44,1) = -999.
	DAT1(79,1) = -999.
C
	DO 111 L=200,320
	DAT1(L,1) = -999.
  111   CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Then call HSTOPR to activate special value checking
C
      ARRAY(1) = -999.
      ARRAY(2) = 1.E-6
      CALL HSTOPR('MV=ON',ARRAY,2)
C
C  Call HSTOPL with NMV = ON, so normalization is by NPTS-MISS.
C
      CALL HSTOPL('NM=ON')
C
C  Call HSTOPL with PMV = ON, so NPTS and MISS are written on the plot.
C
      CALL HSTOPL('PM=ON')
C
C  Turn on the printing of the option list.
C
      CALL HSTOPL('LI=ON')
C
C  Print a main title.
C
      CALL HSTOPC('TIT=ON','Demonstration plot for Histogram.  Special v
     +alue = -999.',0,0)
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C     Plot 2:   IFLAG = 1, input values are sorted into a defined set of
C               8 classes.
C
      IFLAG = 1
      NCLASS = 8
      CLASS(1) = -0.6
      DO 550 I = 1,NCLASS
        CLASS(I+1) = CLASS(I) + 0.20
550   CONTINUE
C
C  Create some input data.
C
      X = 0.
      DO 500 I = 1,NPTS
        DAT1(I,1) = SIN(X)
        X = X + .02
  500 CONTINUE
C
C Put in some special values
C
	DAT1(7,1)  = -999.
	DAT1(44,1) = -999.
	DAT1(79,1) = -999.
C
	DO 112 L=100,220
	DAT1(L,1) = -999.
  112   CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Then call HSTOPR to activate special value checking
C
      ARRAY(1) = -999.
      ARRAY(2) = 1.E-6
      CALL HSTOPR('MV=ON',ARRAY,2)
C
C  Call HSTOPL with NMV = OFF so normalization is relative to NPTS.
C
      CALL HSTOPL('NM=OF')
C
C  Call HSTOPL with PMV = ON, so NPTS and MISS are written on the plot.
C
      CALL HSTOPL('PM=ON')
C
      CALL HSTOPC('TIT=ON','Demonstration plot for Histogram.  Normalize
     + to the original number of points.',0,0)
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
C     Plot 3:   IFLAG = 1, define the CLASS values so as to capture
C               the missing value counts.
C
      IFLAG = 1
      NCLASS = 7
C
C  Set the first class bin between -1000. and -0.6.  Missing values
C   of -999. will fall into this bin.
C
      CLASS(1) = -1000.
      CLASS(2) = -0.6
      DO 555 I = 2,NCLASS
	CLASS(I+1) = CLASS(I) + 0.30
555   CONTINUE
      NPTS = 320
C
      DO 106 I=1,NPTS
	DO 106 J=1,2
          DAT1(I,J)=0.
  106 CONTINUE
C
C  Define some data
C
      X = 0.
      DO 600 I = 1,NPTS
        DAT1(I,1) = SIN(X)
        X = X + .02
  600 CONTINUE
C
C Put in some special values
C
	DAT1(7,1)  = -999.
	DAT1(44,1) = -999.
	DAT1(79,1) = -999.
C
	DO 113 L=200,320
	DAT1(L,1) = -999.
  113   CONTINUE
C
C  (First call HSTOPL('DEF=ON') to activate all default options.)
C
      CALL HSTOPL('DE=ON')
C
C  Label the histogram bar endpoints
C
      CALL HSTOPL('MI=OF')
C
C  Convert the class label format to F7.1
C
      CALL HSTOPC('FOR=ON','(F7.1)',9,7)
C
C  To see the missing values in a histogram bar.  Choose CLASS
C    values so that the missing value indicator becomes a bar
C    and use MVA = OFF (default.)
C
      CALL HSTOPC('TIT=ON','Demo plot for special value in Histogram.
     + Title length has been increased to 96 characters.',0,0)
C
      CALL HISTGR(DAT1, NDIM, NPTS, IFLAG, CLASS, NCLASS, WORK, NWRK)
C
      IERROR = 0
      WRITE (6,1001)
C
      RETURN
C
 1001 FORMAT ('  HISTGR TEST SUCCESSFUL',24X,
     1        'SEE PLOT TO VERIFY PERFORMANCE')
      END
