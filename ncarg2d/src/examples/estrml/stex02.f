
      PROGRAM STEX02
C
C Example STEX02 demonstrates how both the field flow utilities -
C Vectors and Streamlines - map into the non-uniform coordinate
C systems supported by the NCAR Graphics SET call. Each of the
C five frames display a uniform 45 degree field using a Streamlines
C representation overlaying a Vectors representation. The first four
C frames cycle through all possible settings of the Linear-Log (LL)
C argument to the SET call. The fifth frame shows a user coordinate
C system where the unit X is twice the size of the unit Y.
C
C The example also illustrates use of the compatibility mode parameter
C to allow use of the older interfaces (VELVCT and STRMLN), while
C still accessing post-Version 3.2 capabilities of the field flow
C utilities. Note that use of the old entry-points is discouraged
C other than on a transitional basis. Therefore the comments show the
C code required to create an identical plot using the Version 3.2 
C interfaces.
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
C Open GKS, open workstation of type 1, activate workstation
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID, 0, 0.0, 0.00, 0.0)
      CALL GSCR(IWKID, 1, 1.0, 1.00, 1.0)
      CALL GSCR(IWKID, 3, 0.0, 1.00, 0.0)
      CALL GSCR(IWKID, 7, 0.5, 0.15, 0.5)
C
C Invoke demo driver
C
      CALL DOST02(IERR)
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE DOST02 (IERROR)
C
C
      PARAMETER (M=25,N=25)
      DIMENSION U(M,N), V(M,N)
      DIMENSION WRK(M*N*2)
C
C Specify the NDC coordinates for a plot title.
C
        DATA FX / 0.090909 /, FY / 0.976540 /
C
C Specify VELVCT arguments.
C
      DATA FLO/0./,HI/0./,NSET/0/,LENGTH/0/,ISPV/0/,SPV/0./
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Specify velocity field component arrays U and V.
C
      DO  20 I=1,M
         DO  10 J=1,N
            U(I,J)= 1.0
            V(I,J)= 1.0
   10    CONTINUE
   20 CONTINUE
C
C Generate five frames:
C Frame 1: X-Axis linear, Y-Axis linear, equal units
C Frame 2: X-Axis linear, Y-Axis log
C Frame 3: X-Axis log, Y-Axis linear
C Frame 4: X-Axis log, Y-Axis log
C Frame 5: X-Axis linear, Y-Axis linear, 1 X unit = 2 Y units
C
      DO 100 I = 1,5
C           
         IF (I .EQ. 5) THEN
            XMX=50.0
            LL=1
         ELSE
            XMX=100.0
            LL=I
         ENDIF
C
C Set up the user coordinate system and the data coordinate
C boundaries.
C
         CALL SET(0.05,0.95,0.05,0.95,1.0,XMX,1.0,100.0,LL)
         CALL VVSETR('XC1 -- Lower X Bound', 1.0)
         CALL VVSETR('XCM -- Upper X Bound', XMX)
         CALL VVSETR('YC1 -- Lower X Bound', 1.0)
         CALL VVSETR('YCN -- Upper Y Bound', 100.0)
         CALL STSETR('XC1 -- Lower X Bound', 1.0)
         CALL STSETR('XCM -- Upper X Bound', XMX)
         CALL STSETR('YC1 -- Lower X Bound', 1.0)
         CALL STSETR('YCN -- Upper Y Bound', 100.0)
C
C Set the compatibility mode parameters: 
C (1) negative to allow use of Version 3.2 mapping routines 
C (the old mapping routines, FX and FY, do not support non-uniform 
C coordinates, and in addition, must be custom coded to support the
C data coordinate to user coordinate mapping); 
C (2) to absolute value less than 3 to cause the option input 
C arguments for VELVCT and STRMLN (FLO,HI,NSET,LENGTH,ISPV, and SPV) 
C to override the equivalent Version 3.2 parameters; 
C (3) to an even value, specifying that old common blocks be ignored.
C
C This setting causes the value of the NSET parameter to
C determine whether the utilities perform a SET call. If NSET = 1 
C the utilities do not perform the set call. 
C
C ====================================================================
         CALL VVSETI('CPM -- Compatibility Mode', -2) 
         CALL STSETI('CPM -- Compatibility Mode', -2) 
         NSET=1
         CALL GSPLCI(3)
         CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
         CALL GSPLCI(7)
         CALL STRMLN (U,V,WRK,M,M,N,NSET,IER)
C ====================================================================
C
C To produce the same plot using Version 3.2 interfaces, comment out
C the code between this comment and the preceeding one, and uncomment
C the following code (Note that here CPM is left at its default value
C of 0 and the SET parameter is given the value of 0 to specify that
C the utilities should not perform a SET call):
C
C You could try setting the transformation type parameter, TRT, to 0 
C to see the effect it has on the plot frames.
C
C         CALL VVSETI('TRT - Transfomation Type', 0)
C         CALL STSETI('TRT - Transfomation Type', 0)
C
C ====================================================================
C$$$         IDM=0
C$$$         RDM=0
C$$$         CALL VVSETI('SET - Do-SET-Call Flag', 0)
C$$$         CALL STSETI('SET - Do-SET-Call Flag', 0)
C$$$         CALL GSPLCI(3)
C$$$         CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
C$$$         CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
C$$$         CALL GSPLCI(7)
C$$$         CALL STINIT(U,M,V,M,RDM,IDM,M,N,WRK,2*M*N)
C$$$         CALL STREAM(U,V,RDM,IDM,IDM,WRK)
C ====================================================================
C
C Save the current normalization transformation then set it to 0
C
         CALL GQCNTN(IERR,ICN)
         CALL GSELNT(0)
         X = CFUX(FX)
         Y = CFUY(FY)
C
C Call PLCHLQ to write the plot title.
C
         CALL PLCHLQ (X,Y,
     1           'Streamlines Plotted Over a Uniform Vector Field',
     2           16.,0.,-1.)
C
C Restore the normalization transformation
C
         CALL GSELNT(ICN)
         CALL FRAME
C
 100  CONTINUE
C
      IERROR = 0
      RETURN
C
      END
