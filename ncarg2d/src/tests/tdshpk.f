
        PROGRAM TDSHPK
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
C Open GKS, open workstation of type 1, activate workstation.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Invoke demo driver.
C
        CALL DSHPK (IERR,IWKID)
C
C Deactivate and close workstation, close GKS.
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

      SUBROUTINE DSHPK (IERR,IWID)
C
C Declare arrays in which to put coordinates for calls to DPCURV.
C
        DIMENSION XCRA(1001),YCRA(1001)
C
C Declare an array in which to define the bits of an integer dash
C pattern (which saves one from having to do the binary to decimal
C conversion).
C
        DIMENSION IBTS(14)
C
C Define the fourteen bits of an integer dash pattern to be used.
C
        DATA IBTS / 1,0,1,1,0,1,1,1,0,1,1,1,1,0 /
C
C Define multiplicative constants to get from degrees to radians and
C vice-versa.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Define some colors to use.
C
        CALL GSCR   (IWID,0,0.,0.,0.)
        CALL GSCR   (IWID,1,1.,1.,1.)
        CALL GSCR   (IWID,2,1.,0.,1.)
        CALL GSCR   (IWID,3,1.,1.,0.)
C
C Define the mapping from the user system to the fractional system for
C the first frame.
C
        CALL SET (.03,.97,.01,.95,-10.,10.,-10.,10.,1)
C
C Put a label at the top of the first frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +               'DEMONSTRATING THE USE OF DASHPACK - FRAME 1',
     +                                                  .015,0.,0.)
C
C Use the default character dash pattern to draw a box whose edges
C are straight lines and put a label in the middle of the box.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 1111111111111111 (binary)
C or '$$$$$$$$$$$$$$$$' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1., 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        YCEN=8.5
C
        CALL DPLINE (-9.,YCEN+.75, 9.,YCEN+.75)
        CALL DPLINE ( 9.,YCEN+.75, 9.,YCEN-.75)
        CALL DPLINE ( 9.,YCEN-.75,-9.,YCEN-.75)
        CALL DPLINE (-9.,YCEN-.75,-9.,YCEN+.75)
C
        CALL PLCHHQ (0.,YCEN,'A box drawn using DPLINE and the default c
     +haracter dash pattern.',
     +                                                        .01,0.,0.)
C
C Redefine the character dash pattern, draw a second box whose edges
C are straight lines, and put a label in the middle of the box.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 1111111111111111 (binary)
C or '$$$$$$$$$$$$$$$$' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1., 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$$$_$$$A')
C
C Note: At this point, 'DPS' = 0, 'DPT' = 1111111111111111 (binary)
C or '$$$_$$$A' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1., 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        YCEN=6.0
C
        CALL DPLINE (-9.,YCEN+.75, 9.,YCEN+.75)
        CALL DPLINE ( 9.,YCEN+.75, 9.,YCEN-.75)
        CALL DPLINE ( 9.,YCEN-.75,-9.,YCEN-.75)
        CALL DPLINE (-9.,YCEN-.75,-9.,YCEN+.75)
C
        CALL PLCHHQ (0.,YCEN,'A box drawn using DPLINE and a simple char
     +acter dash pattern.',
     +                                                        .01,0.,0.)
C
C Use a 14-bit binary dash pattern to draw a third box whose edges are
C straight lines and put a label in the middle of the box.  Note that
C the routine IPKBTS, which packs the bits of the integer dash pattern
C into an integer variable, is not a part of DASHPACK, but of this
C example.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 1111111111111111 (binary)
C or '$$$_$$$A' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1., 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETI ('DPS - DASH PATTERN SELECTOR',-14)
C
        CALL DPSETI ('DPT - DASH PATTERN (BINARY)',IPKBTS(IBTS,14))
C
C Note: At this point, 'DPS' = -14, 'DPT' = 01011011101111 (binary)
C or '$$$_$$$A' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1., 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        YCEN=3.5
C
        CALL DPLINE (-9.,YCEN+.75, 9.,YCEN+.75)
        CALL DPLINE ( 9.,YCEN+.75, 9.,YCEN-.75)
        CALL DPLINE ( 9.,YCEN-.75,-9.,YCEN-.75)
        CALL DPLINE (-9.,YCEN-.75,-9.,YCEN+.75)
C
        CALL PLCHHQ (0.,YCEN,'A box drawn using DPLINE and a 14-bit bina
     +ry dash pattern.',
     +                                                        .01,0.,0.)
C
C Draw an oval using DPCURV and a character dash pattern in which there
C are no breakpoints.  Smoothing is off by default.
C
C Note: At this point, 'DPS' = -14, 'DPT' = 01011011101111 (binary)
C or '$$$_$$$A' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1., 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETI ('DPS - DASH PATTERN SELECTOR',0)
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',
     +                                    '$$$$$$$$$$$$W/O BREAKPOINTS')
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$$$$W/O BREAKPOINTS' (character), 'LTL' = 0, 'MFS' = 1,
C 'PCF' = 0, 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1.,
C 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        YCEN=0.5
C
        DO 101 I=1,19
          ANGD=20.*REAL(I-1)
          XCRA(I)=9.*COS(DTOR*ANGD)
          YCRA(I)=.11*(9.**6-XCRA(I)**6)**(1/6.)
          IF (ANGD.GT.180.) YCRA(I)=-YCRA(I)
          YCRA(I)=YCEN+YCRA(I)
  101   CONTINUE
C
        CALL DPCURV (XCRA,YCRA,19)
C
        CALL PLCHHQ (0.,YCEN,'An oval drawn using DPCURV, with smoothing
     + off and a:C:dash pattern in which the labels have no breakpoints.
     +',
     +                                                        .01,0.,0.)
C
C Draw a second oval using DPCURV and a character dash pattern in which
C there are breakpoints.  Reduce the added space to be left in each
C label gap.  Smoothing is still off.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$$$$W/O BREAKPOINTS' (character), 'LTL' = 0, 'MFS' = 1,
C 'PCF' = 0, 'SAF' = 360, 'SCF' = 0, 'SSL' = .01, 'TCS' = -1.,
C 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',
     +                       '$$$$$$$$$W|I|T|H| |B|R|E|A|K|P|O|I|N|T|S')
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$W|I|T|H| |B|R|E|A|K|P|O|I|N|T|S' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 0, 'SSL' = .01,
C 'TCS' = -1., 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        YCEN=-3.5
C
        DO 102 I=1,19
          ANGD=20.*REAL(I-1)
          XCRA(I)=9.*COS(DTOR*ANGD)
          YCRA(I)=.11*(9.**6-XCRA(I)**6)**(1/6.)
          IF (ANGD.GT.180.) YCRA(I)=-YCRA(I)
          YCRA(I)=YCEN+YCRA(I)
  102   CONTINUE
C
        CALL DPCURV (XCRA,YCRA,19)
C
        CALL PLCHHQ (0.,YCEN,'An oval drawn using DPCURV, with smoothing
     + off and:C:a dash pattern in which the labels have breakpoints.',
     +                                                        .01,0.,0.)
C
C Draw a third oval in the same way as the second one, but instead of
C embedding break characters in the label, turn on the single-character
C flag.  Turn on the smoother and use even less added space around each
C piece of the broken label.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$W|I|T|H| |B|R|E|A|K|P|O|I|N|T|S' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 0, 'SSL' = .01,
C 'TCS' = -1., 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',
     +                       '$$$$$$$$$$WITH SINGLE-CHARACTER FLAG SET')
C
        CALL DPSETI ('SCF - SINGLE-CHARACTER FLAG',1)
C
        CALL DPSETR ('TCS - TENSION ON CUBIC SPLINES',2.5)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$$WITH SINGLE-CHARACTER FLAG SET' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        YCEN=-7.5
C
        DO 103 I=1,19
          ANGD=20.*REAL(I-1)
          XCRA(I)=9.*COS(DTOR*ANGD)
          YCRA(I)=.11*(9.**6-XCRA(I)**6)**(1/6.)
          IF (ANGD.GT.180.) YCRA(I)=-YCRA(I)
          YCRA(I)=YCEN+YCRA(I)
  103   CONTINUE
C
        CALL DPCURV (XCRA,YCRA,19)
C
        CALL PLCHHQ (0.,YCEN,'An oval drawn using DPCURV, with smoothing
     + on and the:C:single-character flag set to create many breakpoints
     +.',
     +                                                        .01,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Put a label at the top of the second frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +               'DEMONSTRATING THE USE OF DASHPACK - FRAME 2',
     +                                                  .015,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the upper left quadrant of the frame.
C
        CALL SET (.030,.485,.495,.950,-10.,10.,-10.,10.,1)
C
C Use DPFRST, DPVECT, and DPCURV to draw a spiral.  The label will
C follow the curve, because the single-character flag is still on, and
C the curve will be smoothed, because the smoother is still turned on.
C The additional space around the label pieces is reduced even more.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$$WITH SINGLE-CHARACTER FLAG SET' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',
     +                   '$$$$$$$A SPIRAL DRAWN USING DPFRST/VECT/LAST')
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$A SPIRAL DRAWN USING DPFRST/VECT/LAST' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPFRST (0.,0.)
C
        DO 104 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=8.*REAL(I-1)/50.
          CALL DPVECT(RRHO*COS(THTA),RRHO*SIN(THTA))
  104   CONTINUE
C
        CALL DPLAST
C
C Put a small label below the spiral.
C
        CALL PLCHHQ (0.,-9.,'Using DPFRST, DPVECT, and DPLAST.',
     +                                                .01,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the upper right quadrant of the frame.
C
        CALL SET (.515,.970,.495,.950,-10.,10.,-10.,10.,1)
C
C Use DPDRAW to draw another spiral.  Note that the single-character
C flag is still on, so the label follows the curve.  Note also that,
C even though the smoother is still on, it has no effect on DPDRAW.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$A SPIRAL DRAWN USING DPFRST/VECT/LAST' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',
     +             '$_$_$_$_$_$_$_$_$_$_$A SPIRAL DRAWN USING DPDRAW')
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$_$_$_$_$_$_$_$A SPIRAL DRAWN USING DPDRAW' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPDRAW (CUFX(0.),CUFY(0.),0)
C
        DO 105 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=6.28318530717958*REAL(I-1)/20.
          CALL DPDRAW (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  105   CONTINUE
C
        CALL DPDRAW (0.,0.,2)
C
C Put a small label below the spiral.
C
        CALL PLCHHQ (0.,-9.,'Using DPDRAW.',.01,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the lower left quadrant of the frame.
C
        CALL SET (.030,.485,.010,.455,-10.,10.,-10.,10.,1)
C
C Use DPSMTH to draw the same spiral.  The single-character flag is
C still on and smoothing is still on.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$_$_$_$_$_$_$_$A SPIRAL DRAWN USING DPDRAW' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)',
     +               '$_$_$_$_$_$_$_$_$_$_$A SPIRAL DRAWN USING DPSMTH')
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$_$_$_$_$_$_$_$A SPIRAL DRAWN USING DPSMTH' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSMTH (CUFX(0.),CUFY(0.),0)
C
        DO 106 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=6.28318530717958*REAL(I-1)/20.
          CALL DPSMTH (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  106   CONTINUE
C
        CALL DPSMTH (0.,0.,2)
C
C Put a small label below the spiral.
C
        CALL PLCHHQ (0.,-9.,'Using DPSMTH.',.01,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the lower right quadrant of the frame.
C
        CALL SET (.515,.970,.010,.455,-10.,10.,-10.,10.,1)
C
C Use DPSMTH to draw another spiral.  This time, use PLCHHQ function
C codes in the label string and use color to distinguish the label
C from the line.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$_$_$_$_$_$_$_$A SPIRAL DRAWN USING DPSMTH' (character),
C 'LTL' = 0, 'MFS' = 1, 'PCF' = 0, 'SAF' = 360, 'SCF' = 1, 'SSL' = .01,
C 'TCS' = 2.5, 'WOC' = .01, 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$$$$$$$$$$$$$$$$$
     +$$$$$C|o|n|t|o|u|r| |l|e|v|e|l| |=| |1|3|.|6|2|:L1:4|1|0:S:14:N:')
C
        CALL DPSETI ('LTL - LINE-THROUGH-LABEL FLAG',1)
C
        CALL DPSETI ('SCF - SINGLE-CHARACTER FLAG',0)
C
        CALL DPSETR ('SSL - SMOOTHED SEGMENT LENGTH',.001)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$$$$$$$$$$$$$$C|o|n|t|o|u|r| |l|e|v|e|l| |=| |1|3|.|6|2|:L
C 1:4|1|0:S:14:N:' (character), 'LTL' = 1, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL GSPLCI (2)
        CALL PCSETI ('CC - CHARACTER COLOR',3)
C
        CALL DPSMTH (CUFX(0.),CUFY(0.),0)
C
        DO 107 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=6.28318530717958*REAL(I-1)/15.
          CALL DPSMTH (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  107   CONTINUE
C
        CALL DPSMTH (0.,0.,2)
C
C Put a small label below the spiral.
C
        CALL GSPLCI (1)
        CALL PCSETI ('CC - CHARACTER COLOR',1)
C
        CALL PLCHHQ (0.,-9.,'Using PLCHHQ function codes and colors.',
     +                                                      .01,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Put a label at the top of the third frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.975),
     +               'DEMONSTRATING THE USE OF DASHPACK - FRAME 3',
     +                                                  .015,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the upper left quadrant of the frame.
C
        CALL SET (.030,.485,.495,.950,-10.,10.,-10.,10.,1)
C
C Use DPSMTH to draw a spiral.  Use color to distinguish the labels
C from the line, and orient all the labels horizontally.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$$$$$$$$$$$$$$$C|o|n|t|o|u|r| |l|e|v|e|l| |=| |1|3|.|6|2|:L
C 1:4|1|0:S:14:N:' (character), 'LTL' = 1, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 0, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$$$$$$$$SPIRAL')
C
        CALL DPSETI ('SAF - STRING-ANGLE FLAG',-360)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$SPIRAL' (character), 'LTL' = 1, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = -360, 'SCF' = 0, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL GSPLCI (2)
        CALL PCSETI ('CC - CHARACTER COLOR',3)
C
        CALL DPSMTH (CUFX(0.),CUFY(0.),0)
C
        DO 108 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=8.*REAL(I-1)/50.
          CALL DPSMTH (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  108   CONTINUE
C
        CALL DPSMTH (0.,0.,2)
C
C Put a small label below the spiral.
C
        CALL GSPLCI (1)
        CALL PCSETI ('CC - CHARACTER COLOR',1)
C
        CALL PLCHHQ (0.,-9.,'Using horizontal labels.',.01,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the upper right quadrant of the frame.
C
        CALL SET (.515,.970,.495,.950,-10.,10.,-10.,10.,1)
C
C Use DPDRAW to draw a spiral.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$$SPIRAL' (character), 'LTL' = 1, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = -360, 'SCF' = 0, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$$$$$$$SPIRAL')
C
        CALL DPSETI ('LTL - LINE-THROUGH-LABEL FLAG',0)
C
        CALL DPSETI ('PCF - PLOTCHAR FLAG',1)
C
        CALL DPSETI ('SAF - STRING-ANGLE FLAG',360)
C
        CALL DPSETI ('SCF - SINGLE-CHARACTER FLAG',1)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$SPIRAL' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 1,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPDRAW (CUFX(0.),CUFY(0.),0)
C
        DO 109 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=8.*REAL(I-1)/50.
          CALL DPDRAW (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  109   CONTINUE
C
        CALL DPDRAW (0.,0.,2)
C
C Put a small label below the spiral.
C
        CALL PLCHHQ (0.,-9.,'Using PLCHMQ instead of PLCHHQ.',.01,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the lower left quadrant of the frame.
C
        CALL SET (.030,.485,.010,.455,-10.,10.,-10.,10.,1)
C
C Use DPDRAW to draw a spiral.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$$$SPIRAL' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 1,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .01,
C 'WOG' = .005, and 'WOS' = .005.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$_$_$_$SPIRAL')
C
        CALL DPSETI ('PCF - PLOTCHAR FLAG',0)
C
        CALL DPSETR ('WOC - WIDTH OF CHARACTERS',.02)
C
        CALL DPSETR ('WOG - WIDTH OF GAP',.01)
C
        CALL DPSETR ('WOS - WIDTH OF SOLID',.01)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$SPIRAL' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .02,
C 'WOG' = .01, and 'WOS' = .01.
C
        CALL DPDRAW (CUFX(0.),CUFY(0.),0)
C
        DO 110 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=8.*REAL(I-1)/50.
          CALL DPDRAW (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  110   CONTINUE
C
        CALL DPDRAW (0.,0.,2)
C
C Put a small label below the spiral.
C
        CALL PLCHHQ (0.,-9.,'Changing character and solid/gap sizes.',
     +                                                      .01,0.,0.)
C
C Define the mapping from the user system to the fractional system in
C such a way as to use only the lower right quadrant of the frame.
C
        CALL SET (.515,.970,.010,.455,-10.,10.,-10.,10.,1)
C
C Use DPDRAW to draw two spirals and then use 'MFS' to offset one.
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$SPIRAL' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .02,
C 'WOG' = .01, and 'WOS' = .01.
C
        CALL DPSETR ('WOC - WIDTH OF CHARACTERS',.008)
C
        CALL DPSETR ('WOG - WIDTH OF GAP',.008)
C
        CALL DPSETR ('WOS - WIDTH OF SOLID',.008)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$_$_$_$SPIRAL' (character), 'LTL' = 0, 'MFS' = 1, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .008,
C 'WOG' = .008, and 'WOS' = .008.
C
C Draw the first spiral.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$$$$$SPIRAL 1')
C
        CALL DPSETR ('MFS - MULTIPLIER FOR FIRST SOLID',1.5)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$SPIRAL 1' (character), 'LTL' = 0, 'MFS' = 1.5, 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .008,
C 'WOG' = .008, and 'WOS' = .008.
C
        CALL DPDRAW (CUFX(0.),CUFY(0.),0)
C
        DO 111 I=2,101
          RRHO=6.*REAL(I-1)/100.
          THTA=8.*REAL(I-1)/50.
          CALL DPDRAW (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  111   CONTINUE
C
        CALL DPDRAW (0.,0.,2)
C
C Draw the second spiral.
C
        CALL DPSETC ('DPT - DASH PATTERN (CHARACTER)','$$$$$SPIRAL 2')
C
        CALL DPSETR ('MFS - MULTIPLIER FOR FIRST SOLID',3.)
C
C Note: At this point, 'DPS' = 0, 'DPT' = 01011011101111 (binary)
C or '$$$$$SPIRAL 2' (character), 'LTL' = 0, 'MFS' = 3., 'PCF' = 0,
C 'SAF' = 360, 'SCF' = 1, 'SSL' = .001, 'TCS' = 2.5, 'WOC' = .008,
C 'WOG' = .008, and 'WOS' = .008.
C
        CALL DPDRAW (CUFX(0.),CUFY(0.),0)
C
        DO 112 I=2,101
          RRHO=8.*REAL(I-1)/100.
          THTA=8.*REAL(I-1)/50.
          CALL DPDRAW (CUFX(RRHO*COS(THTA)),CUFY(RRHO*SIN(THTA)),1)
  112   CONTINUE
C
        CALL DPDRAW (0.,0.,2)
C
C Put a small label below the spirals.
C
        CALL PLCHHQ (0.,-9.,'Using the first-solid multiplier.',
     +                                                      .01,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Done.
C
        WRITE
     +    (6,'(''DASHPACK TEST EXECUTED OKAY - SEE PLOTS TO CERTIFY'')')
C
        IERR=0
C
        RETURN
C
      END



      FUNCTION IPKBTS (IBTS,NBTS)
C
C This value of this function, when given an array of NBTS 0s and 1s,
C is the integer resulting from packing those bits together, to be
C used as an integer dash pattern.
C
        DIMENSION IBTS(NBTS)
C
C Initialize the value of the function to zero.
C
        IPKBTS=0
C
C One at a time, shift the bits by the proper amount and "or" them into
C the value of the function, making sure to use only the lowest-order
C bit of each incoming array element.
C
        DO 101 I=1,NBTS
          IPKBTS=IOR(IPKBTS,ISHIFT(IAND(IBTS(I),1),NBTS-I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
