C
C	$Id: vvumxy.f,v 1.2 1992-12-03 21:37:44 dbrown Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVUMXY (X,Y,U,V,UVM,XB,YB,XE,YE,IST)
C
C This is a user modifiable routine that allows custom projections of
C the vector space. Input is in the user coordinates. Output
C is in fractional coordinates as defined by the SET call.
C Note that this is different from the old MXF and MYF routines, which
C output in 'plotter coordinate' space.
C 
C VVUMXY (Velocity Vector -- User Map X,Y) is called whenever 
C IMAP has a value other than 0,1, or 2.
C
C Based on the magnitude and direction of the vector the start and 
C ending points of the vector are returned in NDC space.
C
C Input parameters:
C
C X,Y   -- vector position in the user coordinate system
C U,V   -- vector components from the U,V arrays for this position
C UVM   -- magnitude of the U,V components (supplied for convenience)
C
C Output parameters:
C
C XB,YB -- starting point of the vector in the user coordinate system
C          as defined by the SET call
C XE,YE -- ending point of the vector in the user coordinate system
C          as defined by the SET call
C IST   -- status results of the mapping: 0 indicates success
C 
C
C The mapping common block: made available to user mapping routines
C
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
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
C Common block variables used:
C
C SXDC  -- scale factor from vector length to user coordinates, X
C SYDC  -- scale factor from vector length to user coordinates, Y
C
C Identity transformation
C
         XB=X
         YB=Y
         XE=X+U*SXDC
         YE=Y+V*SYDC
C
C Done.
C
      RETURN
C
      END

