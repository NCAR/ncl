C
C	$Id: stdraw.f,v 1.1 1993-01-15 23:53:22 dbrown Exp $
C
      SUBROUTINE STDRAW  (U,V,UX,VY,IAM,STUMSL)
C
C This routine draws the streamlines.
C
      DIMENSION  U(IUD1,*)             ,V(IVD1,*)
      DIMENSION  UX(IXDM,IYDN)         ,VY(IXDM,IYDN)
      DIMENSION  IAM(*)
      EXTERNAL STUMSL
C
C Input parameters:
C
C U,V    - Vector component arrays
C UX,UY  - Work arrays
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
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +	              IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ITHN       ,IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCM 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM
C
C Text related parameters
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC 
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
      SAVE /STPAR/, /STCOM/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
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
C --------------------------------------------------------------------
C
C Local declarations
C
C Point and list buffers
C
C The XLS and YLS arrays serve as a circular list. they
C are used to prevent lines from crossing one another.
C
      DIMENSION PX(IPNPTS), PY(IPNPTS)
      DIMENSION XLS(IPLSTL), YLS(IPLSTL)
C
C Statement functions for spatial and velocity transformations.
C (If the user wishes other transformations  replace these statement
C functions with the appropriate new ones, or , if the transforma-
C tions are complicated delete these statement functions
C and add external routines with the same names to do the trans-
C forming.)
C
C     FX(X,Y) = X
C     FY(X,Y) = Y
C
C Parameters:
C
C IPZERO, IPONE, IPTWO - the numbers 0,1,2
C PRZERO - the number 0.0
C PTHREE - the number 3.0
C
      PARAMETER (IPZERO=0, IPONE=1, IPTWO=2, PRZERO=0.0, PTHREE=3.0)
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
C I,J      - Grid indices
C UIJ,VIJ  - individual vector components
C CVF      - component-wise vector normalizing factor
C LST      - flag indicating the last point in a streamline
C IUX      - integer storage for retrieved bits
C ISV, JSV - saved grid indices where stream starts in + direction
C NBX      - count of grid boxes for current streamline
C LBC      - box checking variable
C ICT      - count of iterations in current streamline
C X, Y     - current X,Y coordinates (grid coordinates
C XST, YST - Starting X,Y values for the streamline, grid coordinates
C DU, DV   - Current normalized interpolated vector components
C XWO, YWO - Current position in world coordinates
C XUS, YUS - Current position in user coordinates
C XND, YND - Current position in NDC space
C XDS, YDS - value of XND and YND saved at the start of the streamline 
C                           and after each progress check
C XD1, YD1 - Previous position in NDC space
C TA       - The tangent angle in NDC space
C DUV      - The differential normalized interpolated vector magnitude
C CSA,SNA  - Cosine and sine of the tangent angle
C XD2,YD2  - The previous previous position in NDC space
C TMG      - Temporary magnitude 
C LI       - Index into circular crossover checking list
C
C --------------------------------------------------------------------
C Initialize local variables.
C If not using the FX,FY routines, then the vector normalization
C value is fixed. 
C
      IF (ICPM.LT.1) THEN
         VNML=1.0/PTHREE
      ELSE
         VNML=RVNL
      END IF
      VSM = R1MACH(3)*VNML
C
      ISK = I1MACH(5) - 2
      IS1 = ISK + 1
      DFMG=RDFM*FW2W
      SSP=RSSP*FW2W
      CDS=RCDS*DFMG
      LCT=0
      ITO=0
C
      LCU = 1
      LCK = 1
      XLS(1) = 0.
      YLS(1) = 0.
      IDR = 0
      SGN = 1.
      IPC = 0
C
C
C Compute the X and Y normalized (and possibly transformed)
C displacement components (UX and VY).
C
      DO  40 J=IYD1,IYDN
         DO  30 I=IXD1,IXDM
C
            CALL STMPUV(U(I,J),V(I,J),UIJ,VIJ,IST)
            IF (UIJ.NE.0. .OR. VIJ.NE.0.) THEN
               CVF = VNML/SQRT(UIJ*UIJ + VIJ*VIJ)
               UIJ = CVF*UIJ
               VIJ = CVF*VIJ
            END IF
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
C
            CALL SBYTES(UIJ,IPZERO,ISK,2,0,1)
C
            IF (MOD(I,ISGD).NE.0 .OR. MOD(J,ISGD).NE.0) THEN
               CALL SBYTES(UIJ,IPONE,IS1,1,0,1)
            END IF
            IF (MOD(I,IAGD).NE.0 .OR. MOD(J,IAGD).NE.0) THEN
               CALL SBYTES(UIJ,IPONE,ISK,1,0,1)
            END IF
C
            UX(I,J) = UIJ
            VY(I,J) = VIJ
C
 30      CONTINUE
 40   CONTINUE
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
      LCT=LCT+1
      LST=0
C
C First ensure that the point buffer is clear
C
      IF (IPC .GT. 1) CALL STLINE(PX,PY,IPC,IAM,STUMSL)
C
C Find an available box for starting a streamline.
C
      IF (IDR .EQ. 0) THEN
C
         DO  70 J=IYD1,IYM1
            DO  60 I=IXD1,IXM1
               CALL GBYTES(UX(I,J),IUX,ISK,2,0,1)
               IF (IAND(IUX,IPONE) .EQ. IPZERO) GO TO 80
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
         IF (ISVF .NE. 0) THEN
            CALL STSVCK(U,V,I,J,IST)
            IF (IST .NE. 0) GO TO 50
         END IF
C
         ISV = I
         JSV = J
         IDR = 1
         SGN = +1.0
C
      ELSE
C
C Come to here to draw in the opposite direction
C
         IDR = 0
         SGN = -1.
         I = ISV
         J = JSV
      END IF
C
C Initiate the drawing sequence, resetting counters.
C Start all streamlines in the center of a box.
C Find the initial normalized interpolated vector components.
C
      NBX = 0
      ITO = ITO+ICT
      ICT = 0
      IF (IDR.NE.0) LBC = LCK+1
      IF (LBC.GT.IPLSTL) LBC = 1
      X = FLOAT(I)+0.5
      Y = FLOAT(J)+0.5
      XST = X
      YST = Y
      CALL  STDUDV(UX,VY,I,J,X,Y,DU,DV)
      XWO=XLOV+(X-1.0)*XGDS
      YWO=YLOV+(Y-1.0)*YGDS
      DU=DU*SGN
      DV=DV*SGN
C
C Get initial point in the various coordinate systems
C and the tangent angle of the stream. If the compatibility flag
C is positive the FX,FY routines must be used.
C
      IF (ICPM.LE.0) THEN
C
         XWO=XLOV+(X-1.0)*XGDS
         YWO=YLOV+(Y-1.0)*YGDS
         CALL STMPXY(XWO,YWO,XUS,YUS,IST)
         IF (IST .LT. 0) GO TO 50
         XND=CUFX(XUS)
         YND=CUFY(YUS)
         XDS=XND
         YDS=YND
         XD1=XND
         YD1=YND
         CALL STMPTA(XWO,YWO,XUS,YUS,XND,YND,DU,DV,TA,IST)
         IF (IST .LT. 0) GO TO 50
C
      ELSE
C
         XUS=FX(X,Y)
         IF (XUS.LT.WXMN .OR. XUS.GT.WXMX) GO TO 50 
         YUS=FY(X,Y)
         IF (YUS.LT.WYMN .OR. YUS.GT.WYMX) GO TO 50 
         XND=CUFX(XUS)
         YND=CUFY(YUS)
         TA=ATAN2(DV,DU)
C
      END IF
C
      IPC=1
      PX(IPC)=XUS
      PY(IPC)=YUS
C      
C Check grid box directional arrow eligibility
C
      CALL GBYTES(UX(I,J),IUX,ISK,2,0,1)
C
      IF (IDR.NE.0 .AND. IAND(IUX,IPTWO).EQ.0) THEN
         CALL STARDR(XUS,YUS,XND,YND,TA,IAM,STUMSL,IST)
         IF (IST.EQ.0) THEN
            CALL SBYTES(UX(I,J),IPONE,ISK,1,0,1)
         END IF
C
      END IF
C
C Loop to this point until streamline ends
C
 110  CONTINUE
C
C Check to see if the streamline has entered a new grid box.
C
      IF (I.EQ.IFIX(X) .AND. J.EQ.IFIX(Y)) THEN
C
C Must be in same box --  Clear the point buffer if required
C
         IF (IPC .EQ. IPNPTS) THEN
            CALL STLINE(PX,PY,IPNPTS,IAM,STUMSL)
            PX(1)=PX(IPNPTS)
            PY(1)=PY(IPNPTS)
            IPC=1
         ENDIF
C
C Determine the interpolated normalized vector at this point
C
         CALL STDUDV (UX,VY,I,J,X,Y,DU,DV)
         IF (DU.EQ.0.0 .AND. DV.EQ.0.0) GO TO 50
C
C Processing diverges depending on the compatibility mode
C
         IF (ICPM .LE. 0) THEN
C
C Get the tangent angle of the streamline at the current point
C in NDC space
C
            CALL STMPTA(XWO,YWO,XUS,YUS,XND,YND,DU,DV,TA,IST)
            IF (IST.NE.0) GO TO 50
C            
         ELSE
C
C A new point is found in grid space, then transformed into
C user and NDC space. There is no transformation of the tangent
C angle.
            X=X+SGN*DU
            Y=Y+SGN*DV
            XUS=FX(X,Y)
            IF (XUS.LT.WXMN .OR. XUS.GT.WXMX) GO TO 50 
            YUS=FY(X,Y)
            IF (YUS.LT.WYMN .OR. YUS.GT.WYMX) GO TO 50 
            XND=CUFX(XUS)
            YND=CUFY(YUS)
            TA=ATAN2(DV,DU)
C
         END IF
C
C Count the point and add it to the point buffer
C
         ICT=ICT+1
         IPC=IPC+1
         PX(IPC)=XUS
         PY(IPC)=YUS
C
         IF (ICPM.LT.1) THEN
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
C The current point is adjusted one third of the distance back to
C the previous point. Empirically, in most cases, this seems to
C decrease the inaccuracy resulting from the use of a finite valued
C differential step.
C
            XD2=XD1
            YD2=YD1
            XD1=XND
            YD1=YND
            XD1=XD1+(XD2-XD1)/PTHREE
            YD1=YD1+(YD2-YD1)/PTHREE
            XND=XD1+CSA*DFMG*DUV
            YND=YD1+SNA*DFMG*DUV
C
C If the increment takes the line outside the viewport, find an
C interpolated point on the grid edge. Set a flag indicating
C the end of the stream
C
            IF (XND .LT. XVPL) THEN
               XND = XVPL
               IF (ABS(CSA).GT.0.1) THEN
                  TMG = (XND-XD1)/CSA
                  YND = YD1+SNA*TMG
               ENDIF
               LST = 1
            ELSE IF (XND .GT. XVPR) THEN
               XND = XVPR
               IF (ABS(CSA).GT.0.1) THEN
                  TMG = (XND-XD1)/CSA
                  YND = YD1+SNA*TMG
               ENDIF
               LST = 1
            ELSE IF (YND .LT. YVPB) THEN
               YND = YVPB
               IF (ABS(SNA).GT.0.1) THEN
                  TMG = (YND-YD1)/SNA
                  XND = XD1+CSA*TMG
               END IF
               LST = 1
            ELSE IF (YND .GT. YVPT) THEN
               YND = YVPT
               IF (ABS(SNA).GT.0.1) THEN
                  TMG = (YND-YD1)/SNA
                  XND = XD1+CSA*TMG
               END IF
               LST = 1
            END IF
C
C Now that the new point has been found in NDC space, find its
C coordinates in user, world, and grid space.
C
            XUS=CFUX(XND)
            YUS=CFUY(YND)
C
            CALL STIMXY(XUS,YUS,XWO,YWO,IST)
            IF (IST.NE.0) GO TO 50
C
            X=(XWO-XLOV)/XGDS+1.0
            Y=(YWO-YLOV)/YGDS+1.0
C
         END IF
C
C Check streamline progress every 'ICKP' iterations.
C
         IF (MOD(ICT,ICKP).EQ.0) THEN
            IF (ABS(XND-XDS).LT.CDS 
     +           .AND. ABS(YND-YDS).LT.CDS) THEN
               GO TO 50
            END IF
            XDS=XND
            YDS=YND
            GO TO 110
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
C (1) Are the new points on the grid?
C (2) Check for missing data if msg data flag (ISVF) has been set.
C (3) Is this box eligible for a directional arrow?
C (4) Location of this entry versus other streamline entries
C
         I = IFIX(X)
         J = IFIX(Y)
         NBX = NBX+1
C
C Check (1) (Only performed in compatibility mode)
C
         IF (ICPM.GT.0) THEN
            IF (I.LT.IXD1 .OR. I.GT.IXM1 
     +           .OR. J.LT.IYD1 .OR. J.GT.IYM1) THEN
               GO TO  50
            END IF
         END IF
C
C Check (2)
C
         IF (ISVF.NE.0) THEN
            CALL STSVCK(U,V,I,J,IST)
            IF (IST .NE. 0) GO TO 50
         END IF
C
C Check (3)
C
         CALL GBYTES(UX(I,J),IUX,ISK,2,0,1)
         IF (IAND(IUX,IPTWO) .EQ. 0) THEN
            CALL STARDR(XUS,YUS,XND,YND,TA,IAM,STUMSL,IST)
            IF (IST .EQ. 0) THEN
               CALL SBYTES(UX(I,J),IPONE,ISK,1,0,1)
            END IF
         END IF
C
      END IF
C
C Check (4) (performed any time streamline crossover is checked)
C
      DO 140 LI=1,LCU
         IF (ABS(XND-XLS(LI)) .LE. SSP .AND.
     +        ABS(YND-YLS(LI)) .LE. SSP) THEN
            IF (LBC.LE.LCK .AND.
     +           (LI.LT.LBC .OR. LI.GT.LCK)) THEN
               GO TO 50
            ELSE IF (LBC.GT.LCK .AND. 
     +              (LI.LT.LBC .AND. LI.GT.LCK)) THEN
               GO TO 50
            END IF
         END IF
 140  CONTINUE
C
      LCU = MIN0(LCU+1,IPLSTL)
      LCK = LCK+1
      IF (LCK.GT.IPLSTL) LCK = 1
      XLS(LCK) = XND
      YLS(LCK) = YND
      CALL SBYTES(UX(I,J),IPONE,IS1,1,0,1)
      IF (NBX.GE.5) THEN
         LBC = LBC+1
         IF (LBC.GT.IPLSTL) LBC = 1
      END IF
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
C Plot statistics
C
      IF (ISST.EQ.1) THEN
         WRITE(*,*) 'STREAM Statistics'
         WRITE(*,*) '                Streamlines plotted:',LCT
         WRITE(*,*) '      Total differential step count:',ITO
         WRITE(*,*) ''
      END IF
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STARDR(XUS,YUS,XND,YND,TA,IAM,STUMSL,IST)
C
C This routine draws the arrow. Calculations are in fractional
C coordinates to ensure uniform arrows irrespective of the 
C mapping in effect.
C A small fraction of the differential change is used to find the
C tangent angle at the current position. Once the angle is known the
C arrow can be drawn at a fixed size independent of the mapping
C routine currently employed.
C
C Input parameters:
C
C XUS,YUS - current position in user space
C XND,YND - current position in NDC space
C TA    - Angle in NDC
C IAM   - Area mask array
C STUMSL - User defined masked streamline drawing routine
C
C Output parameters:
C
C IST - Status code, indicates success or failure
C
      DIMENSION  IAM(*)
      EXTERNAL STUMSL
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
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +	              IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ITHN       ,IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCM 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM
C
C Text related parameters
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC 
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
      SAVE /STPAR/, /STCOM/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
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
C --------------------------------------------------------------------
C
C
C Point buffers
C
      DIMENSION AX(3), AY(3)
C
C Local variables
C
C AX, AY   - Arrow head point buffers
C DXW, DYW - Change in X,Y in window coordinates
C XF, YF   - Arrow head position in the fractional system
C DXF,DYF  - Incremental change in the fractional system
C PHI      - Tangent angle
C K        - Loop index and sign factor for each edge of the arrow
C KK       - Index for the arrow head array, within the loop
C D30      - Half the angle of the point of the arrow head (about 30 o)
C XX,YY    - Ends of the arrow in window coordinates
C
C Parameters:
C
C PHFANG - Half the angle of the arrow head (0.5 in radians is 
C          approximately equivalent to 30 degrees)
C PLWFCT - Linewidth factor, arrow size is increased by this 
C          much when the linewidth is greater than 1.0

      PARAMETER (PHFANG=0.5, PLWFCT=0.15)
C
C ---------------------------------------------------------------------
C
      IST=0
C
      AX(2) = XUS
      AY(2) = YUS
      FLW = 1.0 + PLWFCT*MAX(0,WDLV-1.0)
C
      DO 10 K = -1,1,2
C
C K serves as a sign determining factor; KK indexes the point array.
C
         KK=K+2
         D30 = -(P1D2PI-TA)+FLOAT(K)*PHFANG
         XX = +RNDA*FLW*SIN(D30)+XND
         YY = -RNDA*FLW*COS(D30)+YND
         AX(KK) = CFUX(XX)
         AY(KK) = CFUY(YY)
C
 10   CONTINUE
C
      CALL STLINE(AX,AY,3,IAM,STUMSL)
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STLINE(X,Y,IPC,IAM,STUMSL)
C
C Input parameters:
C
C X,Y - Point arrays
C IPC - Number of points
C IAM   - Area mask array
C STUMSL - User-defined masked streamline drawing routine
C
      DIMENSION X(IPC), Y(IPC)
      DIMENSION  IAM(*)
      EXTERNAL STUMSL
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
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +	              IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ITHN       ,IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCM 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM
C
C Text related parameters
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC 
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
      SAVE /STPAR/, /STCOM/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
C
      DIMENSION IAI(IPGRCT),IAG(IPGRCT)
      DIMENSION XO(IPNPTS), YO(IPNPTS)
C
C ---------------------------------------------------------------------
C
      IF (IMSK.LT.1) THEN
         CALL CURVE(X,Y,IPC)
      ELSE
         CALL ARDRLN(IAM, X, Y, IPC, XO, YO, IPC, 
     +        IAI, IAG, IPGRCT, STUMSL)
      END IF
C
C Done
C 
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STSVCK(U,V,I,J,IST)
C
      DIMENSION  U(IUD1,*), V(IVD1,*)
C
C Checks for special values in the vicinity of I,J
C
C Input parameters
C
C U,V - vector field components array
C I,J - current array position
C
C Output parameters:
C
C IST - status value, 0 if no special values in neighborhood
C
C ---------------------------------------------------------------------
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
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +	              IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ITHN       ,IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCM 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM
C
C Text related parameters
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC 
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
      SAVE /STPAR/, /STCOM/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
C
      IST = 0
C
      IF (U(I,J).EQ.RUSV) THEN
         IST = -1
      ELSE IF (U(I,J+1).EQ.RUSV) THEN
         IST = -1
      ELSE IF (U(I+1,J).EQ.RUSV) THEN
         IST = -1
      ELSE IF (U(I+1,J+1).EQ.RUSV) THEN
         IST = -1
      ELSE IF (V(I,J).EQ.RVSV) THEN
         IST = -1
      ELSE IF (V(I,J+1).EQ.RVSV) THEN
         IST = -1
      ELSE IF (V(I+1,J).EQ.RVSV) THEN
         IST = -1
      ELSE IF (V(I+1,J+1).EQ.RVSV) THEN
         IST = -1
      END IF
C
C Done
C
      RETURN
      END
C
C ---------------------------------------------------------------------
C
      SUBROUTINE STMPUV(UI,VI,UO,VO,IST)
C
C Maps the U,V vector component values
C
C Input parameters:
C
C UI,VI  - Input values of U,V
C
C Output parameters:
C
C UO,VO  - Output mapped component values
C IST    - Status value
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
      PARAMETER (IPLVLS = 64)
C
C Integer and real common block variables
C
C
      COMMON / STPAR /
     +                IUD1       ,IVD1       ,IPD1       ,
     +                IXD1       ,IXDM       ,IYD1       ,IYDN       ,
     +                IXM1       ,IYM1       ,IXM2       ,IYM2
     +                IWKD       ,IWKU       ,ISET       ,IERR       ,
     +	              IXIN       ,IYIN       ,IMSK       ,ICPM       ,
     +                NLVL       ,IPAI       ,ICTV       ,WDLV       ,
     +                UVMN       ,UVMX       ,PMIN       ,PMAX       ,
     +                ITHN       ,IPLR       ,ISST       ,
     +                ICLR(IPLVLS)           ,TVLU(IPLVLS)
C
      COMMON / STTRAN /
     +                UVPS       ,
     +                UVPL       ,UVPR       ,UVPB       ,UVPT       ,
     +                UWDL       ,UWDR       ,UWDB       ,UWDT       ,
     +                UXC1       ,UXCM       ,UYC1       ,UYCM 
C
C Stream algorithm parameters
C
      COMMON / STSTRM /
     +                ISGD       ,IAGD       ,RARL       ,ICKP       ,
     +                ICKX       ,ITRP       ,ICYK       ,RVNL       ,
     +                ISVF       ,RUSV       ,RVSV       ,RNDA       ,
     +                ISPC       ,RPSV       ,RCDS       ,RSSP       ,
     +                RDFM
C
C Text related parameters
C
      COMMON / STTXP /
     +                FCWM    ,ICSZ    ,
     +                FMNS    ,FMNX    ,FMNY    ,IMNP    ,IMNC  ,
     +                FMXS    ,FMXX    ,FMXY    ,IMXP    ,IMXC  ,
     +                FZFS    ,FZFX    ,FZFY    ,IZFP    ,IZFC  ,
     +                FILS    ,FILX    ,FILY    ,IILP     IILC 
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
      SAVE /STPAR/, /STCOM/, /STTRAN/, /STSTRM/, /STTXP/, /STCHAR/
C
C Internal buffer lengths
C
C IPNPTS - Number of points in the point buffer -- not less than 3
C IPLSTL - Streamline-crossover-check circular list length
C IPGRCT - Number of groups supported for area masking
C
      PARAMETER (IPNPTS = 10, IPLSTL = 750, IPGRCT = 64)
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
C --------------------------------------------------------------------
C
C Statement functions for field tranformations
C
      FU(X,Y) = X
      FV(X,Y) = Y
C
C ---------------------------------------------------------------------
C
      IST = 0
C
C Input array polar mode
C
      IF (IPLR .LT. 1) THEN
         UT=UI
         VT=VI
      ELSE IF (IPLR .EQ. 1) THEN
         UT = UI*COS(PDTOR*VI)
         VT = UI*SIN(PDTOR*VI)
      ELSE IF (IPLR .GT. 1) THEN
         UT = UI*COS(VI)
         VT = UI*SIN(VI)
      END IF
C
C Allow mapping using FU,FV functions
C
      UO = FU(UT,VT)
      VO = FV(UT,VT)
C
C Done
C
      RETURN
      END
