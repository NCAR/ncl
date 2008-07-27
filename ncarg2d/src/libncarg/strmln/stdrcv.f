C
C       $Id: stdrcv.f,v 1.4 2008-07-27 00:17:28 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE STDRCV  (U,V,UX,VY,P,WRK,IAM,STUMSL)
C
C This routine draws "curly" vectors. It is adapted from STDRAW.
C
      DIMENSION  U(IUD1,*)             ,V(IVD1,*)
      DIMENSION  UX(IXDM,IYDN)         ,VY(IXDM,IYDN)
      DIMENSION  P(IPD1,*)
      DIMENSION  WRK(*)
      DIMENSION  IAM(*)
      EXTERNAL STUMSL
C
C Input parameters:
C
C U,V    - Vector component arrays
C UX,UY  - Work arrays
C P      - Scalar value or magnitude array
C WRK    - work array used for thinning vectors  
C IAM    - Mask array
C STUMSL - User-defined masked streamline drawing routine
C
C The work array has been broken up into two arrays for clarity.  The
C top half of WORK (called UX) will have the normalized (and
C possibly transformed) U components and will be used for book
C keeping.  the lower half of the WORK array (called VY) will
C contain the normalized (and possibly transformed) V components.
C
C ---------------------------------------------------------------------
C
C NOTE:
C Since implicit typing is used for all real and integer variables
C a consistent length convention has been adopted to help clarify the
C significance of the variables encountered in the code for this 
C utility. All local variable and subroutine parameter identifiers 
C are limited to 1,2,or 3 characters. Four character names identify  
C members of common blocks. Five and 6 character variable names 
C denote PARAMETER constants or subroutine or function names.
C
C Declare the ST common blocks.
C
      PARAMETER (IPLVLS = 256)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2       ,
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +                IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCN 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM       ,RSMD       ,RAMD       ,IGBS       ,
     +                ISTM       ,RVRL       ,RVFR       ,RVRM       ,
     +                IVPO       ,RAFR       ,RDMX       ,RDMN
C
C Text related parameters
C Note: graphical text output is not yet implemented for the
C       Streamline utility.
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP    ,IILC 
C
C Character variable declartions
C
      CHARACTER*160 CSTR
      PARAMETER (IPCHSZ=80)
      CHARACTER*(IPCHSZ)  CMNT,CMXT,CZFT,CILT
C
C Text string parameters
C
      COMMON / STCHAR / CSTR,CMNT,CMXT,CZFT,CILT
C
      SAVE /STPAR/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 256, IPLSTL = 750, IPGRCT = 64)
C
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C ---------------------------------------------------------------------
C
C Local declarations
C
C Point and list buffers
C
C The XLS and YLS arrays serve as a circular list. they
C are used to prevent lines from crossing one another.
C
      DIMENSION PX(IPNPTS), PY(IPNPTS), SV(IPNPTS)
c      DIMENSION XLS(IPLSTL), YLS(IPLSTL)
C
C Parameters:
C
C IPZERO, IPONE, IPTWO - the numbers 0,1,2
C PRZERO - the number 0.0
C PTHREE - the number 3.0
C PSMALL - a small floating point number, large enough to be 
C          detectable by any standard processor
C PMXITR - maximum iteration count for figuring when determining
C          the streamline edge
C
      PARAMETER (IPZERO=0, IPONE=1, IPTWO=2, PRZERO=0.0, PTHREE=3.0)
      PARAMETER (PSMALL=0.000001, PMXITR=32)
C
C Local variables
C
C VSM      - A small value in comparison to the normalized vector mag.
C ISK      - Number of bits to skip in bit routines
C IS1      - ISK + 1
C SSP      - Stream spacing value in fractional (ND) coordinates
C CDS      - Critical displacement in fractional (ND) coordinates
C LCT      - Count of streamlines drawn
C ITO      - Total number of points used to draw all the streamlines
C LCU      - Amount of list currently in use
C LCK      - Current list index
C IDR      - drawing direction 0 + direction 1 - direction
C SGN      - multiplier to change sign based on drawing direction
C IPC      - number of points currently in the point buffer
C ICT      - count of iterations in current streamline
C I,J      - Grid indices
C UIJ,VIJ  - individual vector components
C CVF      - component-wise vector normalizing factor
C LST      - flag indicating the last point in a streamline
C IUX      - integer storage for retrieved bits
C ISV, JSV - saved grid indices where stream starts in + direction
C NBX      - count of grid boxes for current streamline
C LBC      - box checking variable
C X, Y     - current X,Y coordinates (grid coordinates
C DU, DV   - Current normalized interpolated vector components
C XDA, YDA - Current position in data coordinates
C XUS, YUS - Current position in user coordinates
C XND, YND - Current position in NDC space
C XNS, YNS - value of XND and YND saved at the start of the streamline 
C                           and after each progress check
C XN1, YN1 - Previous position in NDC space
C TA       - The tangent angle in NDC space
C DUV      - The differential normalized interpolated vector magnitude
C CSA,SNA  - Cosine and sine of the tangent angle
C XN2,YN2  - The previous previous position in NDC space
C TMG      - Temporary magnitude 
C XT,YT    - Temporary x and y values
C XU1,YU1  - Previous X and Y user coordinate values
C NCT      - Iteration count for determining the streamline edge
C LI       - Index into circular crossover checking list
C IZO      - Zero field flag
C VMF      - current magnitude / max magnitude
C VRL      - ndc ref length in fractional coordinates
C VFL      - ndc length of the adjusted minimum vector
C CFR      - current size in fractional coordinates (ndc)
C IPD      - is partially drawn - part of the line has been drawn
C ICN      - cancel line flag
C ICA      - cancel arrow flag
C DS       - current interpolated scalar value
C DST      - current length of curly vector segment
C
C --------------------------------------------------------------------
C
C Initialize local variables.
C
C Bit manipulation values
C
      VSM = R1MACH(3)*VNML
      ISK = I1MACH(5) - 2
      IS1 = ISK + 1
C
C Stream spacing (setting depends on whether grid relative sizing is
C in effect) and critical displacement
C
      IF (IGBS.EQ.0) THEN
         SSP=RSSP*FW2W
      ELSE
         SSP=RSSP*FW2W/REAL(IXDM)
      END IF
      CDS=RCDS*DFMG
      AMD=RAMD*FW2W
C
C Stream and arrow counters
C
      LCT=0
      ITO=0
      IAC=0
C
C Crossover list variables
C
c      LCU = 1
c      LCK = 1
c      XLS(1) = 0.0
c      YLS(1) = 0.0
C
C Current streamline variables
C
      IDR = 0
      SGN = 1.0
      IPC = 0
      ICT = 0
      IUC = 0
      JSV = IYD1
      VMF = 1.0
      DST = 0.0
      XUS = 0.0
      YUS = 0.0
      XND = 0.0
      YND = 0.0
      TA = 0.0
      IPD = 0
      ICN = 0
      ICA = 0
      SV0 = 0.0
C
C
C Compute the X and Y normalized (and possibly transformed)
C displacement components (UX and VY).
C
      IZO = 1
      DO  40 J=IYD1,IYDN
         DO  30 I=IXD1,IXDM
C
            IF (ISVF.NE.0) THEN
               IF (U(I,J).EQ.RUSV .OR. V(I,J).EQ.RVSV) THEN
                  UIJ = 0.0
                  VIJ = 0.0
                  GO TO 25
               END IF
            END IF
C
            CALL STMPUV(U(I,J),V(I,J),UIJ,VIJ,IST)
            IF (UIJ.NE.0. .OR. VIJ.NE.0.) THEN
               IZO = 0
               T = SQRT(UIJ*UIJ + VIJ*VIJ)
               CVF = VNML/T
               UIJ = CVF*UIJ
               VIJ = CVF*VIJ
            END IF
 25         CONTINUE
C
C Bookkeeping is done in the least significant bits of the UX array.
C When UIJ is exactly zero this can present some problems.
C To get around this problem, set it to a relatively small number.
C
            IF (UIJ.EQ.0.0) UIJ = VSM
C
C Mask out the least significant two bits as flags for each grid box
C A grid box is any region surrounded by four grid points.
C Flag 1 indicates whether any streamline has previously passed
C through this box.
C Flag 2 indicates whether any directional arrow has already
C appeared in this box.
C Judicious use of these flags prevents overcrowding of
C streamlines and directional arrows.
C If thinning using the SMD parameter ignore SGD
C
            CALL SBYTES(UIJ,IPZERO,ISK,2,0,1)
C
            IF (RSMD.LE.0) THEN
               IF (MOD(I,ISGD).NE.0 .OR. MOD(J,ISGD).NE.0) THEN
                  CALL SBYTES(UIJ,IPONE,IS1,1,0,1)
               END IF
            END IF
C
            UX(I,J) = UIJ
            VY(I,J) = VIJ
C
 30      CONTINUE
 40   CONTINUE
C
C If Zero field bail out
C
      IF (IZO .EQ. 1) THEN
         LCT = 0
         ITO = 0
         GO TO 190
      END IF
      CALL STILNS(VRL,VFL,IAV)
C
C If thinning the vectors based on the NDC distance between grid
C points set up the thinning arrays
C
      IF (RSMD .GT. 0) THEN
         CALL STTHIN(U,V,P,WRK(1),WRK(IXDM*IYDN+1))
      END IF
C
C
C Start a streamline. Experience has shown that a pleasing picture
C will be produced if new streamlines are started only in grid
C boxes that previously have not had other streamlines pass through
C them. As long as a reasonably dense pattern of available boxes
C is initially prescribed, the order of scanning the grid pts. for
C available boxes is immaterial.
C
 50   CONTINUE
C
C First ensure that the point buffer is clear
C
      IF (IPC .LE. 2  .OR. (IPD .EQ. 0 .AND. DST .LE. PSMALL)) ICN = 1
      IF (ICN .EQ. 1) THEN
         IDR = 0
      ELSE
         CALL STLNSG(PX,PY,SV,IPC,IAM,STUMSL)
         IPD = 1
         IF (ICA .EQ. 0) THEN
            IF (IVPO .LT. 0) THEN
               CALL STARDR(XU0,YU0,X0,Y0,TA0,VMF,SV0,IAM,STUMSL,IST)
            ELSE IF (SGN .GT. 0) THEN
               CALL STARDR(XUS,YUS,XND,YND,TA,VMF,SV(IPC),
     +              IAM,STUMSL,IST)
            END IF
         END IF
      END IF
C
      LST=0
C
C Find an available box for starting a streamline.
C First check to see if the box has already been marked as
C ineligible. Then if thinning is in effect check to see
C if the thinning array rules it out (note that special values
C have been marked in the thinning array). If so, mark it ineligible.
C Otherwise check for a special value, and if positive mark it
C ineligible. 
C
      IF (IDR .EQ. 0) THEN
C
         LCT=LCT+1
         ITO = ITO+ICT
         ICT = 0
         DO  70 J=JSV,IYM1
            DO  60 I=IXD1,IXM1
               CALL GBYTES(UX(I,J),IUX,ISK,2,0,1)
               IF (IAND(IUX,IPONE) .EQ. IPZERO) THEN
                  IF (RSMD .GT. 0) THEN
                     CALL STTHND(I,J,WRK(1),IS)
                     IF (IS .EQ. 0) THEN
                        GO TO 80
                     ELSE
                        CALL SBYTES(UX(I,J),IPONE,IS1,1,0,1)
                     END IF
                  ELSE IF (ISVF .NE. 0) THEN
                     CALL STSVCK(U,V,I,J,IST)
                     IF (IST .EQ. 0) THEN
                        GO TO 80
                     ELSE
                        CALL SBYTES(UX(I,J),IPONE,IS1,1,0,1)
                     END IF
                  ELSE
                     GO TO 80
                  END IF
               END IF
 60         CONTINUE
 70      CONTINUE
C
C Must be no available boxes for starting a streamline.
C This is the final exit from the streamline drawing loop
C
         GO TO 190
C
 80      CONTINUE
C
C Initialize parameters for starting a streamline.
C Turn the box off for starting a streamline.
C If the special value parameter is turned on, check to see if 
C this box has missing data. If so, find a new starting box.
C
         CALL SBYTES(UX(I,J),IPONE,IS1,1,0,1)
         ISV = I
         JSV = J
C
C
C Depending on the vector position flag draw in both directions or only
C in one direction
C
         IF (IVPO .EQ. 0) THEN
            IDR = 1
         END IF
         IF (IVPO .LT. 0) THEN
            SGN = -1.0
         ELSE
            SGN = +1.0
         END IF
         IUC = 0
         DST = 0.0
         IPD = 0
         ICN = 0
         ICA = 0
C
      ELSE
C
C Come to here to draw in the opposite direction
C
         IDR = 0
         SGN = -1.
         I = ISV
         J = JSV
         DST = 0.0
         ITO = ITO+ICT
      END IF
C
C Initiate the drawing sequence, resetting counters.
C Start all streamlines in the center of a box.
C Find the initial normalized interpolated vector components.
C
C      NBX = 0
C      IF (IDR.NE.0) LBC = LCK+1
C      IF (LBC.GT.IPLSTL) LBC = 1
      X = REAL(I)
      Y = REAL(J)
      T = SQRT(U(I,J)*U(I,J) + V(I,J)*V(I,J))
C      if (t .eq. uvmx) then
C         write(*,*) i,j,uvmx
C      end if
      
      VMF = T / UVMX
      IF (IAV .EQ. 0) THEN
         CFR = VMF * RDMX
      ELSE
         CFR = VFL + (RDMX - VFL) * (T - UVMN) / (UVMX - UVMN)
      END IF
      IF (IVPO .EQ. 0) THEN
         CFR = CFR / 2.0
      END IF
      CALL  STDUDV(UX,VY,I,J,X,Y,DU,DV)
C
C Get initial point in the various coordinate systems
C and the tangent angle of the stream. 
C
      XDA=XLOV+(X-1.0)*XGDS
      YDA=YLOV+(Y-1.0)*YGDS
      CALL HLUSTMPXY(XDA,YDA,XUS,YUS,IST)
      IF (IST .LT. 0) GO TO 50
      XND=CUFX(XUS)
      YND=CUFY(YUS)
      XU0 = XUS
      YU0 = YUS
      X0 = XND
      Y0 = YND
      XOL = XND
      YOL = YND
      XN1=XND
      YN1=YND
      CALL HLUSTMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
      IF (IST .LT. 0) GO TO 50
      TA0 = TA
C
      XNS=XND
      YNS=YND
      ICT=1
      IPC=1
      PX(IPC)=XUS
      PY(IPC)=YUS
      IF (ICTV .NE. 0) THEN
         CALL STITSV(P,I,J,X,Y,DS)
         SV(IPC) = DS
         SV0 = DS
      END IF
C
C Loop to this point until streamline ends
C
 110  CONTINUE
C
C Check to see if the streamline has entered a new grid box.
C
      IF (I.EQ.INT(X) .AND. J.EQ.INT(Y)) THEN
C
C Must be in same box --  Clear the point buffer if required
C
         IF (IPC .EQ. IPNPTS) THEN
            CALL STLNSG(PX,PY,SV,IPNPTS,IAM,STUMSL)
            IPD = 1
            PX(1)=PX(IPNPTS)
            PY(1)=PY(IPNPTS)
            IF (ICTV .NE. 0) THEN
               SV(1)=SV(IPNPTS)
            END IF
            IPC=1
         END IF
C
C Determine the interpolated normalized vector at this point
C
         CALL STDUDV (UX,VY,I,J,X,Y,DU,DV)
         IF (DU.EQ.0.0 .AND. DV.EQ.0.0) GO TO 50
C
C Get the tangent angle of the streamline at the current point
C in NDC space
C
         CALL HLUSTMPTA(XDA,YDA,XUS,YUS,XND,YND,DU,DV,TA,IST)
         IF (IST.NE.0) GO TO 50
C
C Count the point and add it to the point buffer
C
         ICT=ICT+1
         IPC=IPC+1
         PX(IPC)=XUS
         PY(IPC)=YUS
         IF (ICTV .NE. 0) THEN
            CALL STITSV(P,I,J,X,Y,DS)
            SV(IPC) = DS
         END IF
C
         IF (LST .EQ. 1) GO TO 50
C
C The increment in NDC space needs to be proportional to the
C magnitude of the interpolated vector, in order to ensure that
C progress checking works at points of convergence or divergence.
C The square enhances the effectiveness of the technique.
C
         DUV=(DU*DU+DV*DV)/(VNML*VNML)
         CSA=COS(TA)*SGN
         SNA=SIN(TA)*SGN
C     
C     The current point is adjusted one third of the distance back to
C     the previous point. Empirically, in most cases, this seems to
C     decrease the inaccuracy resulting from the use of a finite valued
C     differential step.
C     
         XN1=XND-(XND-XOL)/PTHREE
         YN1=YND-(YND-YOL)/PTHREE
         XOL=XND
         YOL=YND
         XND=XN1+CSA*DFMG*DUV
         YND=YN1+SNA*DFMG*DUV
         XD = XND - XOL
         YD = YND - YOL
         DST = DST + SQRT(XD*XD+YD*YD) 
C     
C     If distance is basically 0 at this point we might as well bail out of
C     this line, because we're not going to make any progress.
C     
         IF (DST .LT. 1E-12) THEN
            LST = 1
         END IF
C     
C     If the increment takes the line outside the viewport, find an
C     interpolated point on the grid edge. Set a flag indicating
C     the end of the stream
C     
         IF (XND .LT. XVPL) THEN
            XND = XVPL
            IF (ABS(CSA).GT.0.1) THEN
               TMG = (XND-XN1)/CSA
               YND = YN1+SNA*TMG
            ENDIF
            LST = 1
         ELSE IF (XND .GT. XVPR) THEN
            XND = XVPR
            IF (ABS(CSA).GT.0.1) THEN
               TMG = (XND-XN1)/CSA
               YND = YN1+SNA*TMG
            ENDIF
            LST = 1
         ELSE IF (YND .LT. YVPB) THEN
            YND = YVPB
            IF (ABS(SNA).GT.0.1) THEN
               TMG = (YND-YN1)/SNA
               XND = XN1+CSA*TMG
            END IF
            LST = 1
         ELSE IF (YND .GT. YVPT) THEN
            YND = YVPT
            IF (ABS(SNA).GT.0.1) THEN
               TMG = (YND-YN1)/SNA
               XND = XN1+CSA*TMG
            END IF
            LST = 1
         END IF
         IF (LST .EQ. 1 .AND. DST .LT. CFR .AND. IVPO .GE. 0) THEN
            ICA = 1
         END IF
         IF (DST .GT. CFR) THEN
            LST = 1
         END IF
C     
C     Now that the new point has been found in NDC space, find its
C     coordinates in user, data, and grid space.
C     
         XU1=XUS
         YU1=YUS
         XUS=CFUX(XND)
         YUS=CFUY(YND)
C     
C     Even if the point is within NDC and User boundaries it can still be 
C     outside the data area. In this case we use an iterative technique to
C     determine the end of the streamline.
C     
         CALL HLUSTIMXY(XUS,YUS,XDA,YDA,IST)
         IF (IST.GE.0) THEN
            X=(XDA-XLOV)/XGDS+1.0
            Y=(YDA-YLOV)/YGDS+1.0
         ELSE
            NCT=1
C     
C     Loop to this point dividing the distance in half at each step
C     
 120        CONTINUE
            XT=XU1+(XUS-XU1)/2.0
            YT=YU1+(YUS-YU1)/2.0
            IF (NCT.GE.PMXITR) GO TO 50
            IF (ABS(XUS-XU1).LE.PSMALL .AND. 
     +           ABS(YUS-YU1).LE.PSMALL) THEN
               XUS=XU1
               YUS=YU1
               CALL HLUSTIMXY(XUS,YUS,XDA,YDA,IST)
               IF (IST.LT.0) THEN
                  IF (DST .LT. CFR) THEN
                     ICA = 1
                  END IF
                  GO TO 50
               END IF
            ELSE
               CALL HLUSTIMXY(XT,YT,XDA,YDA,IST)
               NCT=NCT+1
               IF (IST.LT.0) THEN
                  XUS=XT
                  YUS=YT
               ELSE
                  XU1=XT
                  YU1=YT
               END IF
               GO TO 120
            END IF
C     
            XND=CUFX(XUS)
            YND=CUFY(YUS)
            LST=1
            IF (DST .LT. CFR) THEN
               ICA = 1
            END IF
         END IF
C     
C     
C     If on the top or right edge of the grid space, decrease the X and/or
C     Y value by a small amount so the interpolation routine still works.
C     
         IF (INT(X).GE.IXDM) X=REAL(IXDM)-PSMALL
         IF (INT(Y).GE.IYDN) Y=REAL(IYDN)-PSMALL
C
C Check streamline progress every 'ICKP' iterations.
C
         IF (MOD(ICT,ICKP).EQ.0) THEN
            IF (ABS(XND-XNS).LT.CDS 
     +           .AND. ABS(YND-YNS).LT.CDS) THEN
               GO TO 50
            END IF
            XNS=XND
            YNS=YND
         END IF
C
C If the circular list does not need to be checked for
C streamline crossover, return to the top of the main loop.
C
         IF (ICKX.LT.0 .OR. MOD(ICT,ICKX).NE.0) GO TO 110
C
      ELSE
C
C Must have entered a new grid box  check for the following :
C (1) Check for missing data if msg data flag (ISVF) has been set.
C     cancel the line if nothing has been drawn.
C
         I = INT(X)
         J = INT(Y)
C
C Check (1)
C
         IF (ISVF.NE.0) THEN
            CALL STSVCK(U,V,I,J,IST)
            IF (IST .NE. 0) THEN
               IF (IPD .EQ. 0) THEN
                  ICN = 1
               END IF
               GO TO 50
            END IF
         END IF
C
      END IF
C
C Mark the grid box as drawn
C
      CALL SBYTES(UX(I,J),IPONE,IS1,1,0,1)
C
C Return to top of drawing loop
C
      GO TO 110
C
C
C Final exit
C
  190 CONTINUE
C
      IF (IZO .EQ. 1) THEN
         CALL STZERO
      END IF
C
C Plot statistics
C
      IF (ISST.EQ.1) THEN
         LUN=I1MACH(2)
         WRITE(LUN,*) 'STREAM Statistics'
         WRITE(LUN,*) '                Streamlines plotted:',LCT
         WRITE(LUN,*) '      Total differential step count:',ITO
         WRITE(LUN,*) ' '
      END IF
C
C Set the workspace used parameter
C
      IWKU = 2*IXDM*IYDN
C
      RETURN
      END
C
C -------------------------------------------------------------------
C
      SUBROUTINE STILNS(VRL,VFL,IAV)
C
C This subroutine is used only for curly vector mode  
C
C This subroutine initializes a number of variables that control the
C mapping between vector magnitude and the NDC length of the arrow.
C
C Input paramters:
C
C Output parameters:
C
C VRL - length of reference vector in NDC
C IAV - Adjust vector length flag (based on fractional length - VFL)
C VFL - Length in fractional coordinates of the adjusted minimum
C       vector
C
C ---------------------------------------------------------------------
C
C NOTE:
C Since implicit typing is used for all real and integer variables
C a consistent length convention has been adopted to help clarify the
C significance of the variables encountered in the code for this 
C utility. All local variable and subroutine parameter identifiers 
C are limited to 1,2,or 3 characters. Four character names identify  
C members of common blocks. Five and 6 character variable names 
C denote PARAMETER constants or subroutine or function names.
C
C Declare the ST common blocks.
C
      PARAMETER (IPLVLS = 256)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2       ,
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +                IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCN 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM       ,RSMD       ,RAMD       ,IGBS       ,
     +                ISTM       ,RVRL       ,RVFR       ,RVRM       ,
     +                IVPO       ,RAFR       ,RDMX       ,RDMN
C
C Text related parameters
C Note: graphical text output is not yet implemented for the
C       Streamline utility.
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP    ,IILC 
C
C Character variable declartions
C
      CHARACTER*160 CSTR
      PARAMETER (IPCHSZ=80)
      CHARACTER*(IPCHSZ)  CMNT,CMXT,CZFT,CILT
C
C Text string parameters
C
      COMMON / STCHAR / CSTR,CMNT,CMXT,CZFT,CILT
C
      SAVE /STPAR/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 256, IPLSTL = 750, IPGRCT = 64)
C --------------------------------------------------------------------
C
C The mapping common block: made available to user mapping routines
C
      COMMON /STMAP/
     +                IMAP       ,LNLG       ,INVX       ,INVY       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                XGDS       ,YGDS       ,NXCT       ,NYCT       ,
     +                ITRT       ,FW2W       ,FH2H       ,
     +                DFMG       ,VNML       ,RBIG       ,IBIG
C
      SAVE /STMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C         
C
      VRL = RDMX
      VFL = RDMN
      IAV = 0
C
C
C Determine the length of the maximum, minimum and reference
C vector magnitudes in NDC. Once these values are calculated
C the maximum vector length only is used for scaling purposes
C in the VVMPXY routine. Its value is also calculated in user
C coordinates. This value may not be useful to VVMPXY if the 
C user coordinate system is not uniform.
C
C There are a number of ways these lengths may be determined,
C depending on whether the user has specified (1) a 
C reference magnitude, (2) a reference length, or (3) a fractional
C size for the minimum magnitude. 
C
C 
      VFR = MIN(1.0, RVFR)
C
C If the field is uniform, special conditions apply
C
      IF (UVMX - UVMN .LE. 0.0) THEN
         IF (RVRL .GT. 0.0 .AND. RVRM .GT. 0.0) THEN
            VRL=RVRL*FW2W
            RDMX=VRL*UVMX/RVRM
         ELSE IF (RVRM .GT. 0.0) THEN
            VRL=RDMX*RVRM/UVMX
         ELSE IF (RVRL .GT. 0.0) THEN
            RDMX=RVRL*FW2W
            VRL=RDMX
         END IF
         VFL=VRL
         RDMN=VFL
C
C If no reference magnitude specified, the maximum magnitude
C is used as the reference magnitude
C
      ELSE IF (RVRM .LE. 0.0) THEN
         IF (RVRL .GT. 0.0) THEN
            RDMX=RVRL*FW2W
         END IF
         VRL=RDMX
         IF (VFR .GT. 0.0) THEN
            IAV=1
            VFL=VFR*RDMX
            RDMN=VFL
         ELSE
            RDMN=RDMX*(UVMN/UVMX)
            VFL=RDMN
         END IF
C
C If the reference magnitude is less than the minimum magnitude,
C the fractional size is ignored if a reference length is also
C specified. Otherwise, the fractional size determines not the
C minimum magnitude length, but the reference magnitude length.
C
      ELSE IF (RVRM .LE. UVMN) THEN
         IAV=1
         IF (RVRL .GT. 0.0) THEN
            VRL=RVRL*FW2W
            RDMX=VRL*UVMX/RVRM
         ELSE IF (VFR .GT. 0.0) THEN
            VRL=RDMX*VFR
         ELSE
            VRL=RDMX*RVRM/UVMX
         END IF
         RDMN=VRL*UVMN/RVRM
         VFL=RDMN
C
C A reference magnitude is specified, as well as a fractional
C magnitude. The min magnitude length is the fractional size 
C times the reference length. If a reference length is specified, 
C it becomes the length of the reference magnitude.  The maximum
C magnitude is determined proportionally. If no reference length
C is specified, the reference length is set proportionally to
C the default size assigned to the maximum magnitude.
C
      ELSE IF (VFR .GT. 0.0) THEN
         IAV=1
         IF (RVRL .GT. 0.0) THEN
            VRL=RVRL*FW2W
            VFL=VFR*VRL
            RDMN=VFL
            RDMX=RDMN+(VRL-RDMN)*(UVMX-UVMN)/(RVRM-UVMN)
         ELSE
            RAT=(RVRM-UVMN)/(UVMX-UVMN)
            VRL=RDMX*RAT/(1.0-VFR+VFR*RAT)
            VFL=VFR*VRL
            RDMN=VFL
         END IF
C
C A reference magnitude is specified. If a reference length is 
C specified, it becomes the length of the reference magnitude.
C Otherwise the reference magnitude's length is proportional
C to the default length assigned to the maximum magnitude.
C The min magnitude length is then determined proportionally
C to both these values.
C
      ELSE
         IF (RVRL .GT. 0.0) THEN
            VRL=RVRL*FW2W
            RDMX=VRL*UVMX/RVRM
            VFL=VRL*UVMN/RVRM
            RDMN=VFL
         ELSE
            VRL=RDMX*RVRM/UVMX
            VFL=RDMX*UVMN/UVMX
            RDMN=VFL
         END IF
      END IF
C
C Done
C
      RETURN
      END
