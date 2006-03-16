
      PROGRAM TVELVC
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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL TVELVC1 (IERR)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE TVELVC1 (IERROR)
C
C PURPOSE                To provide a simple demonstration of VELVCT.
C
C USAGE                  CALL TVELVC1 (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    If the test is successful, the message
C
C               VELVCT TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      VELVCT
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              This test program calls entries EZVEC and
C                        VELVCT.  Each call produces a plot of a
C                        vector field obtained from the function
C
C                          Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                   -1./((X+.1)**2+Y**2+.09),
C
C                        by using the direction of the Z gradient
C                        vectors and the logarithm of the absolute
C                        value of the components.
C
C HISTORY                Originally written in November 1976.
C                        Converted to FORTRAN 77 and GKS in July 1984.
C
C
      DIMENSION       U(21,25)   ,V(21,25)
C
C Specify coordinates for a plot title.
C
        DATA IX/94/,IY/1000/
C
C Specify VELVCT arguments.
C
      DATA FLO/0./,HI/0./,NSET/0/,LENGTH/0/,ISPV/0/,SPV/0./
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Specify velocity field functions U and V.
C
      M = 21
      N = 25
      DO  20 I=1,M
         X = .1*REAL(I-11)
         DO  10 J=1,N
            Y = .1*REAL(J-13)
            DZDX = 1.-2.*(X-.10)/((X-.10)**2+Y**2+.09)**2+
     1             2.*(X+.10)/((X+.10)**2+Y**2+.09)**2
            DZDY = 1.-2.*Y/((X-.10)**2+Y**2+.09)**2+
     1             2.*Y/((X+.10)**2+Y**2+.09)**2
            UVMAG = ALOG(SQRT(DZDX*DZDX+DZDY*DZDY))
            UVDIR = ATAN2(DZDY,DZDX)
            U(I,J) = UVMAG*COS(UVDIR)
            V(I,J) = UVMAG*SIN(UVDIR)
   10    CONTINUE
   20 CONTINUE
C
        CALL GQCNTN(IERR,ICN)
C
C Select normalization transformation 0.
C
        CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
        X = CPUX(IX)
        Y = CPUY(IY)
      CALL PLCHLQ (X,Y,'DEMONSTRATION PLOT FOR ENTRY EZVEC OF VELVCT',
     1           16.,0.,-1.)
        CALL GSELNT(ICN)
C
C Call EZVEC for a default velocity field plot.
C
      CALL EZVEC (U,V,M,N)
C
C Call VELVCT to generate the user tuned velocity field plot.
C
      CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
        CALL GQCNTN(IERR,ICN)
C
C Select normalization transformation 0.
C
        CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
        X = CPUX(IX)
        Y = CPUY(IY)
      CALL PLCHLQ (X,Y,
     1           'DEMONSTRATION PLOT FOR ENTRY VELVCT OF VELVCT',16.,
     2           0.,-1.)
        CALL GSELNT(ICN)
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' VELVCT TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END

