;
;      $Id: shea_util.ncl,v 1.39 2010-03-08 19:45:31 haley Exp $
;
; ******************** NOTE *******************************
; These are my personal functions/procedures. They do things
; the way I want them. If u want to modify one to suit
; your tastes then I suggest u copy this file or an 
; individual routine, rename it and then modify it as u wish.
; 
; I will try to help people out but these routines are NOT
; meant to be general purpose.
; ********************************************************
;; Users must load shea_util.ncl
;;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
; ********************************************************
; ------------------------------------------------------------
undef("printVarInfo")
procedure printVarInfo (x, xInfo:string)

; basically superceded by NCL function: printVarSummary
; kept for compatibility/historical reasons

; similar to "  print(x)  " except that data values are NOT printed out
; usage:   printVarInfo (omega,"omega")
;          printVarInfo (omega,"omega: DEBUG")
;          printVarInfo (omega,"Any String You Want")
begin
  print (" ")
  print ("==========> printVarInfo: "+xInfo)
                                        
  printVarSummary (x)
 
  if (isnumeric(x)) then
      print ("Minimum: "+min(x)+"   Maximum: "+max(x) )
      print (" ")
  end if
end
; -------------------------------------------------------
undef("FixZeroContour")
function FixZeroContour (CNLVLS[*]:float, label:string)
; called internally
local eps, indEps, nEps, cnlvls, nClvl
begin
       cnlvls   = CNLVLS                          ; historical
       nClvl    = dimsizes(cnlvls)
       if (nClvl.gt.1) then
          ;eps    = 1.e-09                          ; arbitrary
           eps    = 1.e-05                          ; arbitrary
           nEps   = num(fabs(cnlvls).le.eps)
           if (nEps.gt.0 .and. nEps.ne.nClvl) then
               indEps=ind(fabs(cnlvls).le.eps) 
               cnlvls(indEps) = 0.0        
          ;else                            ; debug print
          ;    print (label+": no zero contours")
           end if
       end if 
       return (cnlvls)                                      
end
; -------------------------------------------------------
undef("FixZeroContourEps")
function FixZeroContourEps (CNLVLS[*]:float, label:string, eps:numeric)
; called internally
local indEps, nEps, cnlvls, nClvl
begin
       cnlvls   = CNLVLS                          ; historical
       nClvl    = dimsizes(cnlvls)
       if (nClvl.gt.1) then
           nEps   = num(fabs(cnlvls).le.eps)
           if (nEps.gt.0 .and. nEps.ne.nClvl) then
               indEps=ind(fabs(cnlvls).le.eps) 
               cnlvls(indEps) = 0.0        
          ;else                            ; debug print
          ;    print (label+": no zero contours")
           end if
       end if
       return (cnlvls)                                      
end
;-------------------------------------------------------------------
undef("FixZeroContour_HALEY")
function FixZeroContour_HALEY(LEVELS[*]:numeric, label:string)  
;                                                 ; 18 July 2001 2:55pm
;procedure fix_zero_contour(levels)               ; MARY HALEY ROUTINE
local eps, nlevels, dlevels, n                    ; WITH SHEA MODS
begin 
  levels  = LEVELS 

  eps     = 1.e-05
  nlevels = dimsizes(levels)         ; # of levels

  dlevels = min( levels(1:nlevels-1) - levels(0:nlevels-2) )   ; delta levs
;
; If delta levels is < eps, don't even bother checking the levels.
;
  if(dlevels.ge.eps)
    do n=1,nlevels-2
      if(fabs(levels(n)).le.eps .and. \
         levels(n-1).lt.0..and.levels(n+1).gt.0.)
        levels(n) = 0.0
      end if
    end do
 ;else                            ; debug print
 ;  print (label+": no zero contours")
  end if

  return(levels)
end
; -------------------------------------------------------
undef("GetOverlayPlot")
function GetOverlayPlot (plot:graphic, ClassName:string, PlotIndex:integer)

; Retrieves a specified overlaid object.

; ClassName = name of the type of object you are searching for
;             i.e. "contourPlotClass", "vectorPlotClass"
; PlotIndex = in case there are multiple objects of the same type
;             overlaid, use this to indicate which one to return.
;             PlotIndex = 0  --> first instance
;             PlotIndex = 1  --> second instance, etc
local i, ovrly_ids
begin
;
; Retrieve objects that have been overlaid on "plot".
; 
      getvalues plot
          "pmOverlaySequenceIds" : ovrly_ids
      end getvalues
;
; Loop through these objects and check if any of them are a
; match.
;
      if(.not.any(ismissing(ovrly_ids))) then
          num_instances = 0
          do i=0,dimsizes(ovrly_ids)-1
              if(NhlClassName(ovrly_ids(i)).eq.ClassName)
                if(num_instances.eq.PlotIndex) then
                  return(ovrly_ids(i))
                end if
                num_instances = num_instances + 1
              end if
          end do
      end if
;
; If no match found, then check the plot itself.
;
      if(NhlClassName(plot).eq.ClassName) then
          return(plot)
      end if
;
; No match found, so return a missing object.
;
      dum = new(1,graphic)
      return(dum)
end

;-------------------------------------------------------------------
undef("get_cnLevels")
function get_cnLevels (plot:graphic) 
local cnlvls, ovrly_plot
begin
      ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)

      getvalues ovrly_plot
        "cnLevels"   : cnlvls
      end getvalues

      return(cnlvls)
end
; -------------------------------------------------------
undef("get_flags")
function get_flags (plot:graphic) 
local ovrly_plot
begin
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)

       getvalues ovrly_plot
          "cnLevelFlags"   : flags
       end getvalues
       return(flags)
end
;-------------------------------------------------------------------
undef("ZeroLineContour")
function ZeroLineContour (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line twice as thick 
local cnlvls, cnlinethk, n, N
begin
       cnlvls = get_cnLevels (plot)

       N = dimsizes(cnlvls)
       if (ismissing(N) .or. N.le.0) then
           print ("ZeroLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)               ; no change
       else 
           cnlvls = FixZeroContour (cnlvls, "ZeroLineContour")
       end if

       cnlinethk = new (N, integer)          ; create vector for line patterns
       cnlinethk = 1                         ; default

       do n=0,N-1
          if (cnlvls(n).eq.0.) then
              cnlinethk(n) = 2               ; make the zero contour thicker
          end if
       end do

       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnMonoLineThickness"    : False
           "cnLineThicknesses"      : cnlinethk
       end setvalues

      return (plot)
end
;-------------------------------------------------------------------
undef("ZeroLineContourThick")
function ZeroLineContourThick (plot:graphic, Thick:integer) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line "Thick" as thick 

local cnlvls, cnlinethk, n, N
begin
       cnlvls = get_cnLevels (plot)

       N = dimsizes(cnlvls)
       if (ismissing(N) .or. N.le.0) then
           print ("ZeroLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)               ; no change
       else 
           cnlvls = FixZeroContour (cnlvls, "ZeroLineContour")
       end if

       cnlinethk = new (N, integer)          ; create vector for line patterns
       cnlinethk = 1                         ; default

       do n=0,N-1
          if (cnlvls(n).eq.0.) then
              cnlinethk(n) = Thick           ; make the zero contour thicker
          end if
       end do

       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnMonoLineThickness"    : False
           "cnLineThicknesses"      : cnlinethk
       end setvalues

      return (plot)
end
; -------------------------------------------------------
undef("ZeroNegDashLineContour")
function ZeroNegDashLineContour (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line twice as thick and set neg contourlines to dash
; Dash line patterns:
;    http://www.ncl.ucar.edu/Document/Graphics/dash_patterns.shtml

local cnlvls, cnlinepat, cnlinethk, n, N
begin
       cnlvls = get_cnLevels (plot)

       N = dimsizes(cnlvls)
       if (ismissing(N) .or. N.le.0) then
           print ("ZeroNegDashLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)
       else 
           cnlvls = FixZeroContour (cnlvls, "ZeroNegDashLineContour")
       end if

       if (any(cnlvls.le.0.)) then
           cnlinepat = new (dimsizes(cnlvls), integer) ; line pattern vector
           cnlinepat = 0                               ; default is solid (=0)
           cnlinethk = new (dimsizes(cnlvls), integer) ; line thick vector
           cnlinethk = 1                               ; default

           do n=0,N-1
              if (cnlvls(n).lt.0.) then
                  cnlinepat(n) = 5               ; simple dash line pattern
              end if
              if (cnlvls(n).eq.0.) then
                  cnlinethk(n) = 2               ; make the zero contour thicker
              end if
           end do
    
           ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
           setvalues ovrly_plot
               "cnMonoLineDashPattern"  : False
               "cnLineDashPatterns"     : cnlinepat
               "cnMonoLineThickness"    : False
               "cnLineThicknesses"      : cnlinethk
           end setvalues
      end if                                ; any

      return (plot)

end
; -------------------------------------------------------
undef("ZeroNegDashLineContourOverlay")
function ZeroNegDashLineContourOverlay (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line twice as thick and set neg contourlines to dash
; Dash line patterns:
;   http://www.ncl.ucar.edu/Document/Graphics/dash_patterns.shtml

local cnlvls, cnlinepat, cnlinethk, n, N, ovrly_plot
begin
;
; Get the *second* instance (index=1) of an overlaid contour plot.
;
      ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 1)

      if(.not.ismissing(ovrly_plot)) then
        getvalues ovrly_plot
            "cnLevels"   : cnlvls
        end getvalues

        N = dimsizes(cnlvls)
        if (ismissing(N) .or. N.le.0) then
           print ("ZeroNegDashLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)
        else 
           cnlvls = FixZeroContour (cnlvls, "ZeroNegDashLineContour")
        end if

        if (any(cnlvls.le.0.)) then

           cnlinepat = new (dimsizes(cnlvls), integer) ; line pattern vector
           cnlinepat = 0                               ; default is solid (=0)
           cnlinethk = new (dimsizes(cnlvls), integer) ; line thick vector
           cnlinethk = 1                               ; default

           do n=0,N-1
              if (cnlvls(n).lt.0.) then
                  cnlinepat(n) = 5               ; simple dash line pattern
              end if
              if (cnlvls(n).eq.0.) then
                  cnlinethk(n) = 2               ; make the zero contour thicker
              end if
           end do
    
           setvalues ovrly_plot
               "cnMonoLineDashPattern"  : False
               "cnLineDashPatterns"     : cnlinepat
               "cnMonoLineThickness"    : False
               "cnLineThicknesses"      : cnlinethk
           end setvalues
        end if                                ; any
      end if

      return (plot)

end
; -------------------------------------------------------
undef("ZeroGoneNegDashLineContour")
function ZeroGoneNegDashLineContour(plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Minimizes the zero line and set neg contourlines to dash
; Dash line patterns:
;   http://www.ncl.ucar.edu/Document/Graphics/dash_patterns.shtml
local cnlvls, N, flags, cnlinepat, cnlevflg, n
begin
  cnlvls = get_cnLevels (plot)    ; get contour level by querrying plot
  N = dimsizes(cnlvls)            ; how many contour levels
  flags  = get_flags(plot)        ; get line/label types
  
;
; if there are no contour levels
;
     if (ismissing(N) .or. N.le.0) then
        print ("ERROR in ZeroGoneNegDashLineContour: dimsizes(cnlvls)=" \
                +N+" return (non-fatal)")
        return (plot)
     else 
        cnlvls = FixZeroContour (cnlvls, "ZeroGoneNegDashLineContour")
     end if
;
; set the contour line styles
;
 if (any(cnlvls.le.0.)) then
    cnlinepat = new (dimsizes(cnlvls), integer) ; line pattern vector
    cnlinepat = 0                               ; default is solid (=0)
    cnlevflg  = new (dimsizes(cnlvls), integer)   

    do n=0,N-1
       if (cnlvls(n).lt.0.) then
          cnlinepat(n) = 5               ; simple dash line pattern
       end if
       if(cnlvls(n).eq.0.)then
	  cnlevflg(n) = 0                ; no line or labels
       else
          cnlevflg(n) = flags(n)         ; whatever was set before
       end if
    end do

    ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
    setvalues ovrly_plot
         "cnMonoLineDashPattern"  : False
         "cnLineDashPatterns"     : cnlinepat
         "cnMonoLevelFlag"        : False     ; this is the default
         "cnLevelFlags"           : cnlevflg 
    end setvalues
  end if                                ; any

  return (plot)

end
; -------------------------------------------------------
undef("NegDashLineContour")
function NegDashLineContour (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Set neg contourlines to dash
; Dash line patterns:
;   http://www.ncl.ucar.edu/Document/Graphics/dash_patterns.shtml

local cnlvls, cnlinepat, n, N
begin
       cnlvls = get_cnLevels (plot)

       N = dimsizes(cnlvls)
       if (ismissing(N) .or. N.le.0) then
           print ("NegDashLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)
       else 
           cnlvls = FixZeroContour (cnlvls, "NegDashLineContour")
       end if

       if (any(cnlvls.lt.0.)) then
           cnlinepat = new (dimsizes(cnlvls), integer) ; create vector for line patterns
           cnlinepat = 0                         ; array; default is solid (=0)

           do n=0,dimsizes(cnlvls)-1
              if (cnlvls(n).lt.0.) then
                  cnlinepat(n) = 5               ; simple dash line pattern
              end if
           end do
    
           ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
           setvalues ovrly_plot
               "cnMonoLineDashPattern"  : False
               "cnLineDashPatterns"     : cnlinepat
           end setvalues
       end if                                ; any

      return (plot)

end
; -------------------------------------------------------
undef("ShadeLtContour")
function ShadeLtContour (plot:graphic, CnLt:float, FillPatLt:integer ) 

; operates on a plot object created by "gsn_csm.ncl" code
; Fill all contours less than "CnLt" to the
;      pattern corresponding to "FillPatLt" 
;   http://www.ncl.ucar.edu/Document/Graphics/fill_patterns.shtml

local cnlvls, patterns, i, N
begin
                                        ; Retrieve contour levels.
   cnlvls = get_cnLevels (plot)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeLtContour: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ShadeLtContour")
   end if

   if (any(cnlvls.lt.CnLt)) then
       patterns  = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
       patterns(:)  = -1                           ; patterns and initialize
                                                   ; it to transparent.
       do i=0,N-1                        ; Fill contour levels depending on 
         if(cnlvls(i).lt.CnLt) then      ; different criteria.
           patterns(i) = FillPatLt       ; see above URL
         end if
       end do
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot           ; Set fill patterns 
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
       end setvalues
   end if           
 
   return (plot)
end
; -------------------------------------------------------
undef("ShadeGtContour")
function ShadeGtContour (plot:graphic, CnGt:float, FillPatGt:integer ) 

; operates on a plot object created by "gsn_csm.ncl" code
; Fill all contours greater than "CnGt" to the
;      pattern corresponding to "FillPatGt" 
;      http://www.ncl.ucar.edu/Document/Graphics/fill_patterns.shtml

local cnlvls, patterns, i, N
begin
 
   cnlvls = get_cnLevels (plot)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeGtContour: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ShadeGtContour")
   end if

   if (any(cnlvls.gt.CnGt)) then
       patterns  = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
       patterns(:)  = -1                           ; patterns and initialize
                                                   ; it to transparent.
       
       do i=0,N-1                        ; Fill contour levels depending on 
          if(cnlvls(i).gt.CnGt) then
             patterns(i) = FillPatGt     ; See above URL
          end if
       end do
       patterns(i)  = FillPatGt          ; last pattern
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnFillOn"          : True
           "cnMonoFillColor"   : True
           "cnMonoFillPattern" : False
           "cnFillPatterns"    : patterns
       end setvalues
   end if
 
   return (plot)

end
; -------------------------------------------------------
undef("ShadeLtGtContour")
function ShadeLtGtContour (plot:graphic
                          ,CnLt:float, FillPatLt:integer\
                          ,CnGt:float, FillPatGt:integer ) 

; operates on a contour object created by "gsn_csm.ncl" code
; Fill all contours less-than/greater-than "CnLt/CnGt" to the
;      pattern corresponding to "FillPat" 
;      http://www.ncl.ucar.edu/Document/Graphics/fill_patterns.shtml

local cnlvls, patterns, i, N
begin
 
   cnlvls = get_cnLevels (plot)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeLtGtContour: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ShadeLtGtContour")
   end if

   if (any(cnlvls.lt.CnLt) .or. any(cnlvls.gt.CnGt)) then
       patterns  = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
       patterns(:)  = -1                           ; patterns and initialize
                                                   ; it to transparent.
       
       do i=0,N-1                        ; Fill contour levels depending on 
         if(cnlvls(i).lt.CnLt) then      ; different criteria.
           patterns(i) = FillPatLt       ; see above URL
         end if
          if(cnlvls(i).gt.CnGt) then
             patterns(i) = FillPatGt     ; See above URL
          end if
       end do
       patterns(i)  = FillPatGt          ; last pattern
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnFillOn"          : True
           "cnMonoFillColor"   : True
           "cnMonoFillPattern" : False
           "cnFillPatterns"    : patterns
       end setvalues
   end if
 
   return (plot)

end
; -------------------------------------------------------
undef("ShadeGeLeContour")
function ShadeGeLeContour (plot:graphic
                          ,CnGe:float, CnLe:float, FillPat:integer ) 

; operates on a contour object created by "gsn_csm.ncl" code
; Fill all contours greater-than-eq2/less-than-eq2 "CnGe/CnLe" to the
;      pattern corresponding to "FillPat" 
;      http://www.ncl.ucar.edu/Document/Graphics/fill_patterns.shtml

local cnlvls, patterns, i, N
begin
 
   cnlvls = get_cnLevels (plot)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeGeLeContour: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ShadeGeLeContour")
   end if

   if (any(cnlvls.ge.CnGe) .and. any(cnlvls.le.CnLe)) then
       patterns  = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
       patterns(:)  = -1                           ; patterns and initialize
                                                   ; it to transparent.
       
       do i=0,N-1                                  ; Fill contour levels 
         if(cnlvls(i).ge.CnGe .and. cnlvls(i).le.CnLe) then   
            patterns(i) = FillPat               ; see above URL
         end if
       end do
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnFillOn"          : True
           "cnMonoFillColor"   : True
           "cnMonoFillPattern" : False
           "cnFillPatterns"    : patterns
       end setvalues
   end if
 
   return (plot)

end
; -------------------------------------------------------
undef("ShadeGeLeContourDensity")
function ShadeGeLeContourDensity (plot:graphic ,CnGe:float, CnLe:float \
                                 ,FillPat:integer, FillDensity:float ) 

; operates on a contour object created by "gsn_csm.ncl" code
; Fill all contours greater-than-eq2/less-than-eq2 "CnGe/CnLe" to the
;      pattern corresponding to "FillPat" 
;      http://www.ncl.ucar.edu/Document/Graphics/fill_patterns.shtml

local cnlvls, patterns, i, N
begin
 
   cnlvls = get_cnLevels (plot)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeGeLeContour: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ShadeGeLeContour")
   end if

   if (any(cnlvls.ge.CnGe) .and. any(cnlvls.le.CnLe)) then
       patterns  = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
       patterns(:)  = -1                           ; patterns and initialize
                                                   ; it to transparent.
       
       do i=0,N-1                                  ; Fill contour levels 
         if(cnlvls(i).ge.CnGe .and. cnlvls(i).le.CnLe) then   
            patterns(i) = FillPat               ; see above URL
         end if
       end do
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnFillOn"          : True
           "cnMonoFillColor"   : True
           "cnMonoFillPattern" : False
           "cnFillPatterns"    : patterns
           "cnFillScaleF"      : FillDensity  ; 'density' of lines/periods
       end setvalues
    end if
 
   return (plot)

end
;-------------------------------------------------------------------
                                      ; utility called internally only
undef("genCmapHueRangeInternal")
function genCmapHueRangeInternal (hueLeft, hueRight) 

; generates left and right "hue" values for *predefined* colors
; used internally by the "genCmap" suite of functions

;    hueRange = genCmapHueRangeInternal("Blue","Red") 
;    hueRange = genCmapHueRangeInternal("BlueGreen","YellowRed") 
;    hueRange = genCmapHueRangeInternal(hleft,hright)   

; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also acceptable
;           numbers could also be used for new colors
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are predefined
;           "blue","red","yellow","green","greenblue","yellowred" also acceptable
;           numbers could also be used for new colors

; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
;                   =RedYellow or YellowRed [0]   color wheel traversed
;                   =BlueGreen or GreenBlue [225] differently
local  colorWheel, hleft, hright, hueRange
begin

 colorWheel = 0.     ; =0. means two color map [=1. means multi-color]
 if (isstring(hueLeft) .and. isstring(hueRight) ) then
   if (hueLeft.eq."Red" .or. hueLeft.eq."red") then
       hleft = 0.
   end if
   if (hueLeft.eq."Blue" .or. hueLeft.eq."blue") then
       hleft = 225.
   end if
   if (hueLeft.eq."Green" .or. hueLeft.eq."green") then
       hleft = 135.
   end if
   if (hueLeft.eq."Yellow" .or. hueLeft.eq."yellow") then
       hleft = 67.5             ; was 60.
   end if
   if (hueLeft.eq."RedYellow" .or. hueLeft.eq."redyellow") then
       hleft      = 0.
       colorWheel = 1.
   end if
   if (hueLeft.eq."BlueGreen" .or. hueLeft.eq."bluegreen") then
       hleft      = 247.5
       colorWheel = 1.
   end if

   if (hueRight.eq."Red" .or. hueRight.eq."red") then
       hright = 0.
   end if
   if (hueRight.eq."Blue" .or. hueRight.eq."blue") then
       hright = 225.
   end if
   if (hueRight.eq."Green" .or. hueRight.eq."green") then
       hright = 135.
   end if
   if (hueRight.eq."Yellow" .or. hueRight.eq."yellow") then
       hright = 67.5             ; was 60.
   end if
   if (hueRight.eq."YellowRed" .or. hueRight.eq."yellowred") then
       hright     = 0.
       colorWheel = 1.
   end if
   if (hueRight.eq."GreenBlue" .or. hueRight.eq."greenblue") then
       hright     = 247.5
       colorWheel = 1.
   end if
 else  
   hleft  = fabs(hueLeft)   
   hright = fabs(hueRight)
   if (hueLeft.lt.0. .or. hueRight.lt.0.) then
       colorWheel = 1.
   end if
 end if  

 hueRange    = new ( 3, float)
 hueRange(0) = hleft
 hueRange(1) = hright
 hueRange(2) = colorWheel     ; flag for how to traverse the 
                              ; color wheel
 return (hueRange)

end
;-------------------------------------------------------------------
                                      ; utility called internally only
undef("genCmapColorWheelInternal")
procedure genCmapColorWheelInternal (wks:graphic, ncol:integer, hueRange:float)
local hleft, hright, colorWheel, h, s, v, cmap
begin
   hleft      = fabs(hueRange(0))
   hright     = fabs(hueRange(1))
   colorWheel = hueRange(2)

   hsv        = new ((/ncol,3/),float)  ; keep v part at 1.0 

   sMax       = 0.90                    ; 1.0/0.8 fine also 
   sMin       = 0.15                       
   if (colorWheel.eq.0.) then
       nc2               = ncol/2
       hsv(0:nc2-1,0)    = hleft
       hsv(nc2:ncol-1,0) = hright

       hsv(0:nc2-1,1)    = fspan(sMax,sMin,nc2)
       hsv(nc2:ncol-1,1) = fspan(sMin,sMax,nc2)

       hsv(0:ncol-1,2)   = 1.0
      ;vmax          = 1.00
      ;vmin          = 1.00
      ;v(0:nc2-1)    = fspan(vmax,vmin,nc2)
      ;v(nc2:ncol-1) = fspan(vmin,vmax,nc2)
   else 
       print ("genCmapColorWheelInternal: hleft="+hleft+" hright="+hright)
       hsv           = new ((/ncol,3/),float) 
       hsv(0:ncol-1,0)   = fspan(hleft,hright,ncol)
       hsv(0:ncol-1,1)   = sMax                      ; keep constant
       hsv(0:ncol-1,2)   = 1.0
       print ("genCmapColorWheelInternal: h="+hsv(:,0)+"  s="+s(:,1))
   end if 

   cmap             = new((/ncol+3,3/),float); create array
   cmap(0,:)        = (/1.,1.,1./)           ; white background
   cmap(1,:)        = (/0.,0.,0./)           ; black foreground
   cmap(2:ncol+1,:) = hsvrgb(hsv)
   cmap(  ncol+2,:) = (/0.90, 0.90, 0.90 /)  ; append gray

  ;print ("ncol="+ncol)
  ;print ("h="+hsv(:,0) +"  s="+hsv(:,1)+"  v="+hsv(:,2))
  ;do n=0,ncol+1
  ;   print ("cmap="+n+"  "+cmap(n,0)+"  "+cmap(n,1)+"  "+cmap(n,2))
  ;end do

   gsn_define_colormap(wks, cmap)
end
;-------------------------------------------------------------------
undef("genCmapManualRes")
procedure genCmapManualRes (wks:graphic, res ,hueLeft, hueRight)

; generates RGB values for *predefined* min, max, contour intervals
; Prior to exiting it uses "gsn_define_colormap(wks, cmap)"
;    so the calling routine need NOT do so.
; Shea prototype for MH

; sample usage: (generate and define a specified color map)

;    res  = True
;    res@cnLevelSelectionMode = "ManualLevels"
;    res@cnMinLevelValF       = 0.
;    res@cnMaxLevelValF       = 10.
;    res@cnLevelSpacingF      = 0.5  ; Manually sets the contour levels. 
;    res@cnFillOn             = True   ; Turn on the color fill.
;    res@mpFillOn             = False  ; Turn off the continent gray.
;    
;    genCmapManualRes(wks, res ,"Blue","Red") 
;    genCmapManualRes(wks, res ,"Green","Yellow") 
;    genCmapManualRes(wks, res ,  35 , 190.)     ; 2-color: orange-LightBlue
;    genCmapManualRes(wks, res ,-225.,-359.)     ; blue-green-yellow-red
;    genCmapManualRes(wks, res ,-359.,-225.)     ; red-yellow-green-blue
;    genCmapManualRes(wks, res ,-315.,-135.)     ; magenta-blue-green

; wks     - workstation 
; res     - variable to which attributes were assigned
; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also acceptable
;           numbers could also be used for new colors
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are predefined
;           "blue","red","yellow","green","greenblue","yellowred" also acceptable
;           numbers could also be used for new colors


; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
; note: nCnLvl can also be integer or float

local ier, MnCnLvl, MxCnLvl, CnInt, nCnLvl, nCnLvl, hueRange, ncol, colorWheel
begin
 ier = 0                               ; error checking
 if (typeof(hueLeft).ne.typeof(hueRight) ) then
     ier = 1
     print ("genCmap: hueLeft/hueRight are not the same type: exit")
     return 
 end if

 if (isatt(res,"cnLevelSelectionMode") .and.   \
     res@cnLevelSelectionMode.eq."ManualLevels") then
     if (isatt(res,"cnMinLevelValF") .and. isatt(res,"cnMaxLevelValF") \
                                     .and. isatt(res,"cnLevelSpacingF")) then
         MnCnLvl  = res@cnMinLevelValF
         MxCnLvl  = res@cnMaxLevelValF
         CnInt    = res@cnLevelSpacingF
         nCnLvl   = floattointeger((MxCnLvl-MnCnLvl)/fabs(CnInt)+1.) 

         hueRange = genCmapHueRangeInternal (hueLeft, hueRight) 
         ncol     = nCnLvl+1
         colorWheel = hueRange(2)
         if (colorWheel.eq.0. .and. ncol%2 .eq. 1) then
             ncol = ncol+1  ; require even # colors for 2 color maps
         end if

         genCmapColorWheelInternal (wks, ncol, hueRange)
     else 
         ier = ier+100
     end if
 else
     ier = ier+10
 end if

 if (ier.ne.0) then
     print ("genCmapManualRes: attributes cnLevelSelectionMode/cnMinLevelValF/" \
           +"cnMaxLevelValF/cnLevelSpacingF must be available")
 end if

end
;-------------------------------------------------------------------
undef("genCmapMnMxCnInt")
procedure genCmapMnMxCnInt (wks:graphic \
                           ,MnCnLvl:float, MxCnLvl:float, CnInt:float \
                           ,hueLeft, hueRight)

; generates RGB values for *predefined* min, max, contour intervals
; Prior to exiting it uses "gsn_define_colormap(wks, cmap)"
;    so the calling routine need NOT do so.
; Shea prototype for MH

; sample usage: (generate and define a specified color map)

;    genCmapMnMxCnInt(wks, -10, 35, 5,"Blue","Red") 
;    genCmapMnMxCnInt(wks, -30, 30,10,"Green","Yellow") 
;    genCmapMnMxCnInt(wks, -30, 30,10,"BlueGreen","YellowRed") 
;    genCmapMnMxCnInt(wks, 100,250,10,-225. , -67.)     ; Blue-Green-Yellow
;    genCmapMnMxCnInt(wks, 100,250,10,  35. , 190.)     ; Orange-LightBlue

; wks     - workstation 
; MnCnLvl - minimum contour level [same as cnMinLevelValF]
; MxCnLvl - maximum contour level [same as cnMaxLevelValF]
; CnInt   - Contour interval      [same as cnLevelSpacingF]
; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also acceptable
;           numbers could also be used for new colors
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are predefined
;           "blue","red","yellow","green","greenblue","yellowred" also acceptable
;           numbers could also be used for new colors

; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
; note: nCnLvl can also be integer or float
local ier, nCnLvl, hueRange, ncol 
begin
 ier = 0                               ; error checking
 if (typeof(hueLeft).ne.typeof(hueRight) ) then
     ier = 1
     print ("genCmapMnMxCnInt: hueLeft/hueRight are not the same type: exit")
 end if

 if (CnInt.eq.0.) then
     ier = ier+10
     print ("genCmapMnMxCnInt: CnInt=0 ; exit")
 end if

 if (MnCnLvl.ge.MxCnLvl) then
     ier = ier+100
     print ("genCmapMnMxCnInt: MnCnLvl>=MxCnLvl; exit")
 end if

 if (ier.eq.0) then
     nCnLvl   = floattointeger((MxCnLvl-MnCnLvl)/fabs(CnInt)+1.) 

     hueRange = genCmapHueRangeInternal(hueLeft, hueRight)
     ncol     = nCnLvl+1
     colorWheel = hueRange(2)
     if (colorWheel.eq.0. .and. ncol%2 .eq. 1) then
         ncol = ncol+1  ; require even # colors for 2 color maps
     end if

     genCmapColorWheelInternal (wks, ncol, hueRange)
 end if 

end
;-------------------------------------------------------------------
undef("genCmapMnMxSpan")
procedure genCmapMnMxSpan (wks:graphic, plot:graphic\
                                      , hueLeft, hueRight)

; generates RGB values for *predefined* min, max, contour intervals
; Prior to exiting it uses "gsn_define_colormap(wks, cmap)"
;    so the calling routine need NOT do so.
; Shea prototype for MH

; sample usage: (generate and define a specified color map)

;    genCmapMnMxSpan(wks, plot,"Blue","Red") 
;    genCmapMnMxSpan(wks, plot,"Green","Yellow") 
;    genCmapMnMxSpan(wks, plot,"BlueGreen","YellowRed") 
;    genCmapMnMxSpan(wks, plot,-225. , -67.)     ; Blue-Green-Yellow

; wks     - workstation 
; plot    - grapgic object to be queried for contour information
; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also acceptable
;           numbers could also be used for new colors
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are predefined
;           "blue","red","yellow","green","greenblue","yellowred" also acceptable
;           numbers could also be used for new colors

; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
; note: nCnLvl can also be integer or float

local ier, cnlvls, nCnLvl, hueRange, ncol 
begin
 ier = 0                               ; error checking
 if (typeof(hueLeft).ne.typeof(hueRight) ) then
     ier = 1
     print ("genCmap: hueLeft/hueRight are not the same type: exit")
 end if

 cnlvls = get_cnLevels (plot)

 hueRange = genCmapHueRangeInternal (hueLeft, hueRight) 
 nCnLvl   = dimsizes(cnlvls)
 ncol     = nCnLvl+1
 colorWheel = hueRange(2)
 if (colorWheel.eq.0. .and. ncol%2 .eq. 1) then
     ncol = ncol+1  ; require even # colors for 2 color maps
 end if
;print (hueRange)
 genCmapColorWheelInternal (wks, ncol, hueRange)

end
;-------------------------------------------------------------------
undef("genCmapCnLvl")
procedure genCmapCnLvl (wks:graphic, nCnLvl:integer, hueLeft, hueRight)
; ----------------limited usefulness ----------------------
; This works but an idiosyncracy of the underlying graphics is
; that it starts at cmap(2,:) and cycles thru the colors.
; However, on all succeeding passes it starts at (cmap1,:).

; generates/defines a color map of RGB values for *predefined* color spans
; Prior to exiting it uses "gsn_define_colormap(wks, cmap)"
;    so the calling routine need NOT do so.
; Shea prototype for MH

; sample usage: (generate and define a specified color map)

; res@cnMinLevelValF        = -10.
; res@cnMaxLevelValF        =  12.
; res@cnLevelSpacingF       =   2.
; nCnLvl = (res@cnMaxLevelValF-res@cnMinLevelValF)/res@cnLevelSpacingF+1.
;   or
; nCnLvl = 10
; genCmapCnLv(wks, nCnLvl,"Blue","Red") 
; genCmapCnLv(wks, nCnLvl,"Green","Red") 
; genCmapCnLv(wks, nCnLvl,"Green","Blue") 
; genCmapCnLv(wks, nCnLvl,"Yellow","Green") 
; genCmapCnLv(wks, nCnLvl, 35. , 190.)     ; see color wheel


; wks     - workstation 
; nCnLvl  - specify the number of contour intervals the color
;           table is to span
; hueLeft - "Blue","Red","Yellow","Green","BlueGreen","RedYellow" are predefined
;           "blue","red","yellow","green","bluegreen","redyellow" also acceptable
;           numbers could also be used for new colors
; hueRight- "Blue","Red","Yellow","Green","GreenBlue","YellowRed" are predefined
;           "blue","red","yellow","green","greenblue","yellowred" also acceptable
;           numbers could also be used for new colors


; hueLeft/hueRight  =string, float or integer [both must have same type] 
; hueLeft/hueRight  =red [0]  =yellow [60]  =green [135]  =blue [225]
; note: nCnLvl can also be integer or float

local ier, hueRange, ncol 
begin
 ier = 0                               ; error checking
 if (typeof(hueLeft).ne.typeof(hueRight) ) then
     ier = 1
     print ("genCmapCnLvl: hueLeft/hueRight are not the same type")
 end if

 if (.not.isinteger(nCnLvl) ) then
     ier = 2
     print ("genCmapCnLvl: nCnLvl is not of type integer")
 end if 

 hueRange = genCmapHueRangeInternal (hueLeft, hueRight) 
 ncol     = nCnLvl                                    
 colorWheel = hueRange(2)
 if (colorWheel.eq.0. .and. ncol%2 .eq. 1) then
     ncol = ncol+1  ; require even # colors for 2 color maps
 end if
 genCmapColorWheelInternal (wks, ncol, hueRange)   
end
;-------------------------------------------------------------------
undef("genCmapLb")
procedure genCmapLb (wks:graphic, plot:graphic \  ; changed 4 Aug 99
                    ,xndc:float, yndc:float, csize:float)

; *************************************
; pls use the gsnPanelLabelBar=True resource.
; see: http://www.cgd.ucar.edu/csm/cmps/CSM_Graphics/color.shtml
;      Example 5
; *************************************
; I think it would be better to use
;         gsn_labelbar_ndc

; xndc - left edg of label bar
; yndc - distance from the bottom
; csize- character size (0.015 or so is common)

local cnlvls, colors, lev_labels, lbres
begin
 print ("--------------------------------------------")
 print ("This functionality of this routine has been " + \
        "replaced by the gsn resource gsnPanelLabelBar" )
 print ("see http://www.cgd.ucar.edu/csm/cmps/CSM_Graphics/color.shtml")
 print ("    Example 5")
 print ("--------------------------------------------")
        
 ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)

 getvalues ovrly_plot
     "cnLevels"     : cnlvls
     "cnFillColors" : colors
 end getvalues

;
; Only label every other line in the labelbar.
; replace with lbres@lbLabelStride = 2
;
  lev_labels = new(dimsizes(cnlvls),string)
  do i = 0,dimsizes(cnlvls)-1,2
    lev_labels(i) = cnlvls(i)
    if(i.lt.dimsizes(cnlvls)-1) 
      lev_labels(i+1) = ""
    end if
  end do

  lbres = True
  lbres@lbFillColors       = colors           ; Set the colors to use.
  lbres@lbLabelAlignment   = "InteriorEdges"  ; Only label interior lines.
  lbres@lbLabelFont        = "helvetica-bold" ; Change label font.
  lbres@lbLabelFontHeightF = csize            ; label font height.
  lbres@lbMonoFillPattern  = True             ; Fill each box solid.
  lbres@lbOrientation      = "horizontal"     ; Horizontal labelbar.
  lbres@lbPerimOn          = False            ; Turn off perimeter.
  lbres@vpHeightF          = 0.1              ; Height of labelbar.
  lbres@vpWidthF           = 0.7              ; Width of labelbar.

  gsn_labelbar_ndc(wks,dimsizes(colors),lev_labels,xndc,yndc,lbres)

end
;-------------------------------------------------------------------
undef("genCmapNameCol")
procedure genCmapNameCol (wks:graphic, colors[*]:string)
; This works but an idiosyncracy of the underlying graphics is
; that it starts at cmap(2,:) and cycles thru the colors.
; However, on all succeeding passes it starts at (cmap1,:).

; generates RGB values for input named colors 
; http://www.scd.ucar.edu/zine/99/spring/text/gfx/2.gifs.html
; http://www.scd.ucar.edu/zine/99/spring/text/gfx/rgb.txt

; FYI: Did you know you can build a color table with named colors *and*
;      RGB triplets?  You can have something like:

; cmap = (/"white","black","(/1.,0.,0./)","yellow","(/0.,1.,0./)"/)

; see:   http://www.scd.ucar.edu/zine/99/spring/text/2.color.html

; Prior to exiting it uses "gsn_define_colormap(wks, cmap)"
;    so the calling routine need NOT do so.
; Shea prototype for MH

; sample usage: (generate and define a specified color map)

;    colors = (/ "red", "magenta", ... /)
;    genCmapNameCol(wks, colors) 

; wks     - workstation 
; colors  = a vector of "Named Colors"

local Ncolors, cmapStr
begin

   Ncolors = dimsizes (colors)
   cmapStr = new ( Ncolors+2, string)
   cmapStr(0:1)          = (/"white", "black" /)
   cmapStr(2:Ncolors+1)  = colors
;
; Change the color map for this workstation.
;
  ;gsn_define_colormap(wks, cmapStr) 
   setvalues wks
     "wkColorMap" : cmapStr
   end setvalues

end
; ----------------------------------------------------------
undef("msgValOutline")
function msgValOutline (wks:graphic, plot:graphic, x[*][*]:float)

; When drawing plots with vectors, streamlines which do not
; there are no resourcse that will draw the outline of
; missing values. This function will do that by turning
; off appropriate contour resources. It uses the _FillValues 
; of variable "x" to draw the outline.

local cres, cplot
begin
 ;printVarInfo (x, "x: msgValOutline") ; debug
  cres=True
 ;cres@cnMissingValFillColor = "lightgray" ; can not be used since
                                           ; cnFillOn=False [below]
  cres@cnMissingValPerimOn = True    ; turn on continenatl outlines
  cres@cnLinesOn           = False   ; don't draw contours
  cres@cnLineLabelsOn      = False   ; don't draw line labels
  cres@cnInfoLabelOn       = False   ; don't draw contour info label
  cres@cnFillOn            = False   ; don't color fill contours
  cres@gsnDraw             = False   ; don't draw so we can overlay
  cres@gsnFrame            = False

  cplot = gsn_contour(wks,x,cres)
; =================================================;
; overlay the two plots so we can get continental outlines
; =================================================;
  overlay(plot,cplot)

  return (plot)
end
; ----------------------------------------------------------
undef("infoTimeStamp")
procedure infoTimeStamp ( wks:graphic, chSize:float, FileInfo:string )

; Place text on plot to indicate the data source and time created
; This procedure must be invoked prior to any advance
; frame call.

; examples:
;          (0) wks =  gsn_open_wks("ncgm","bogus") 
;          (1) infoTimeStamp (wks, 0.01, "Bogus File")
;              [plot]
;
;          (2) mssPath = "/SHEA/ECMWF/"
;              mssName = "sample"
;              size    = 0.015
;              infoTimeStamp (wks, size, mssPath+mssName )
;              [plot]

; txJust: http://www.ncl.ucar.edu/Document/Graphics/Resources/tx.shtml#txJust

local yBot, xLeft, xRight, txres, TimeStamp
begin
   yBot                = 0.001
   xLeft               = 0.001
   xRight              = 0.999
                                        ; psWorkStationClass for NCL v6.0.0 and earlier, documentWorkstationClass for v6.1.0 and newer
   if (NhlClassName(wks).eq."psWorkstationClass".or.NhlClassName(wks).eq."documentWorkstationClass") then 
      getvalues wks
         "wkDeviceLowerX"   : ps_xLeft
         "wkDeviceLowerY"   : ps_yLeft
	 "wkDeviceUpperX"   : ps_xRight
      end getvalues
      
      if(ps_xLeft .lt. 0)then           ; 36 is default, 0 is the beginning
         xoff_set = fabs(ps_xLeft/612.) ; 612 is 8.5 * 72 in-1
         xLeft = 0.02941 + xoff_set     ; .02941 is 1/4 of an inch
      else                              ; which is the margin required
	 xLeft = 0.02941                ; when printing postscript 
      end if

      if(ps_xRight .gt. 612)then              ; 576 is default,612 is end
         xoff_set = fabs(1 - (ps_xRight/612.)); 612 is 8.5 * 72 in-1
         xRight= 0.97059 - xoff_set
      else
	 xRight = 0.97059
      end if

      if(ps_yLeft .lt. 0)then         ; 126 is default, 0 is the beginning
         yoff_set = fabs(ps_yLeft/792.) ; 792 is 11  * 72 in-1
         yBot  = 0.02941  + yoff_set
      else
         yBot  = 0.02941
      end if
   end if

   txres               = True         ; additional info
   txres@txFontHeightF = chSize       ; size of meta data
   txres@txJust        = "BottomRight"
   gsn_text_ndc  (wks, FileInfo  , xRight , yBot , txres)

   TimeStamp           = systemfunc( "date" )
   txres@txJust        = "BottomLeft"
   gsn_text_ndc  (wks,"Created: "+TimeStamp , xLeft, yBot, txres)
end
; ----------------------------------------------------------
undef("specx_ci")
function specx_ci (sdof:numeric ,pLow:numeric, pHigh:numeric)

; Calculate the theoretical Markov spectrum and lower and
; upper confidence curves using the lag1 autocorrelation 
; returned as an attribute by the NCL function "specx_anal"

; Sample usage:
;        (1) sdof = specx_anal(x,d,sm,pct) ; see documentation
;        (2) splt = djs_specx_ci(sdof, 0.05, 0.95) 
;        (3) plot = gsn_csm_xy(wks, sdof@frq, splt,res) 

; INPUT
;    sdof  - degrees of freedom returned by "specx_anal"
;    pLow  - lower confidence limit (0.0 < pLow < 1.)
;            a typical value is 0.05
;    pHigh - upper confidence limit (0.0 < pHigh< 1.)
;            a typical value is 0.95

; RETURNED: a 2D array ["splt"] containing 4 curves
;    splt(0,:)   - input spectrum
;    splt(1,:)   - Markov "Red Noise" spectrum
;    splt(2,:)   - lower confidence bound for Markov
;    splt(3,:)   - upper confidence bound for Markov

local twopi, N, df, r, r2, rsq, temp, mkov, sum1, sum2, scale \
    , xLow, xHigh, splt
begin
  twopi = 2.*3.14159
  N     = dimsizes (sdof@spcx)      ; number specx estimates
  df    = (/ sdof /)

  r     = sdof@xlag1
  r2    = 2.*r
  rsq   = r*r

  temp  = r2*cos(twopi*sdof@frq)    ; vector
                                    
  mkov  = 1./(1. + rsq - temp)      ; Markov Model

  sum1  = sum (mkov)                ; sum Markov elements
  sum2  = sum (sdof@spcx)           ; sum spectral elements
  scale = sum2/sum1                 ; scaling factor

  xLow  = chiinv (pLow,  df)/df     ; lower confidence
  xHigh = chiinv (pHigh, df)/df     ; upper confidence

  splt      = new ( (/4,N/), typeof(sdof)) ; 4 spec curves
  splt(0,:) = sdof@spcx             ; input spectrum
  splt(1,:) = mkov*scale            ; Markov
  splt(2,:) = splt(1,:)*xLow        ; low  ci for Markov
  splt(3,:) = splt(1,:)*xHigh       ; high ci for Markov

  return (splt)
end
; ----------------------------------------------------------
undef("specxy_ci")
function specxy_ci (sdof:numeric ,pLow:numeric, pHigh:numeric)

; Calculate the theoretical Markov spectrum and lower and
; upper confidence curves using the lag1 autocorrelation 
; returned as an attribute by the NCL function "specxy_anal"

; Sample usage:
;        (1) sdof = specxy_anal(x,d,sm,pct) ; see documentation
;        (2) splt = specxy_ci(sdof, 0.05, 0.95) 
;        (3) plotx= gsn_csm_xy(wks, sdof@frq, splt(0:3,:),res) 
;        (4) ploty= gsn_csm_xy(wks, sdof@frq, splt(4,7,:),res) 

; INPUT
;    sdof  - degrees of freedom returned by "specx_anal"
;    pLow  - lower confidence limit (0.0 < pLow < 1.)
;            a typical value is 0.05
;    pHigh - upper confidence limit (0.0 < pHigh< 1.)
;            a typical value is 0.95

; RETURNED: a 2D array ["splt"] containing 8 curves
;    splt(0,:)   - input "x" spectrum
;    splt(1,:)   - x: Markov "Red Noise" spectrum
;    splt(2,:)   - x: lower confidence bound for Markov
;    splt(3,:)   - x: upper confidence bound for Markov
;    splt(4,:)   - input "y" spectrum
;    splt(5,:)   - y: Markov "Red Noise" spectrum
;    splt(6,:)   - y: lower confidence bound for Markov
;    splt(7,:)   - y: upper confidence bound for Markov

local N, df, twopi, temp, xLow, xHigh, splt, nxy, K, r, sum2 \
    , r2, rsq, mkov, sum1, scale
begin
  N     = dimsizes (sdof@spcx)      ; number specx estimates
  df    = (/ sdof /)
  twopi = 2.*3.14159

  xLow  = chiinv (pLow,  df)/df     ; lower confidence
  xHigh = chiinv (pHigh, df)/df     ; upper confidence

  splt  = new ( (/8,N/), typeof(sdof))     ; 8 spec curves

 do nxy=0,1 
  if (nxy.eq.0) then
      K     = 0
      r     = sdof@xlag1
      sum2  = sum (sdof@spcx)       ; sum spectral elements [x]
      splt(K+0,:) = sdof@spcx       ; input spectrum
  else
      K     = 4
      r     = sdof@ylag1
      sum2  = sum (sdof@spcy)       ; sum spectral elements [y]
      splt(K+0,:) = sdof@spcy       ; input spectrum
  end if

  r2    = 2.*r
  rsq   = r*r
  temp  = r2*cos(twopi*sdof@frq)    ; vector
  mkov  = 1./(1. + rsq - temp)      ; Markov Model
  sum1  = sum (mkov)                ; sum Markov elements
  scale = sum2/sum1                 ; scaling factor

  splt(K+1,:) = mkov*scale          ; Markov
  splt(K+2,:) = splt(K+1,:)*xLow    ; low  ci for Markov
  splt(K+3,:) = splt(K+1,:)*xHigh   ; high ci for Markov
 end do                         

  return (splt)
end
; ------------------------------------------------------------
undef("add90LatX")
function add90LatX (Data[*][*]:numeric)

; This assumes that the "x" coordinate is latitude.
; One natural use for this is when using "gsn_csm_pres_hgt"
;                                         ^^^^^^^^^^^^^^^^
; add two bogus X points. The rightmost dimension is the "x" dimension
; Basically, the "x" Coordinate Array will be expanded by two
; points. One will prepend, the other will post-pend.

; add two bogus pole points [ie 90S and 90N] to model 
; (data where the is no pole points) to get nice round
; -90 [90S] and 90 [90N] points


local dims,newData,ny  ,mx  ,mx2, att_names, i
begin
	dims    = dimsizes(Data)
        ny      = dims(0)
        mx      = dims(1)
        mx2     = mx+2

	newData = new((/ny  ,mx2/),typeof(Data))

	newData(:,1:mx2-2) = (/ Data  /)      ; pass values only

        att_names =getvaratts(Data)           ; copy attributes of Data
        if(.not.all(ismissing(att_names)))
                do i = 0,dimsizes(att_names)-1
                        newData@$att_names(i)$ = Data@$att_names(i)$
                end do
        end if
        delete (att_names)

        if (.not.isatt(newData,"_FillValue")) then
            newData@_FillValue = 1.e20        ; arbitrarily assign
        end if

        newData(:,0)     = newData@_FillValue ; bogus end points
        newData(:,mx2-1) = newData@_FillValue
                                              ; copy 0th coordinate info
                                              ; pressure or hgt
        if (.not.ismissing(Data!0) .and. iscoord(Data,Data!0)) then
            newData!0        = Data!0         ; copy the dimension name
            newData&$Data!0$ = Data&$Data!0$  ; copy the coord variables
        end if
                                              ; copy 1th coordinate info
                                              ; latitude here
        if (.not.ismissing(Data!1) .and. iscoord(Data,Data!1)) then
            newData!1 = Data!1                ; copy dimension name
            oldLat    = Data&$Data!1$         ; create a temp 1D array
            newLat    = new ( mx2, typeof(oldLat) )
            newLat(1:mx) = oldLat             ; copy to newLat [no end pts, yet]
            if (newLat(1).lt.0.) then         ; which order are coordinates
                newLat(0)    = -90            ; add an end point
                newLat(mx2-1)=  90            ; add the 2nd end point
            else
                newLat(0)    =  90            ; add an end point
                newLat(mx2-1)= -90            ; add the 2nd end point
            end if

            att_names =getvaratts(oldLat)     ; copy attributes of Lat (if any)
            if(.not.all(ismissing(att_names))) then
                do i = 0,dimsizes(att_names)-1
                        newLat@$att_names(i)$ = oldLat@$att_names(i)$
                end do
            end if
            
            newData&$newData!1$ = newLat
        end if

	return(newData)
end
; ------------------------------------------------------------
undef("add90LatY")
function add90LatY(Data[*][*]:numeric)

; This assumes that the "y" coordinate is latitude.
; One natural use for this is when using "gsn_csm_lat_time".
;                                         ^^^^^^^^^^^^^^^^
; add two bogus Y points. The leftmost dimension is the "y" dimension
; Basically, the "y" Coordinate Array will be expanded by two
; points. One will prepend, the other will post-pend.

; add two bogus pole points [ie 90S and 90N] to model 
; (data where the is no pole points) to get nice round
; -90 [90S] and 90 [90N] points

; A user could take this and change it to mett his/her needs.

local dims,newData,ny  ,mx  ,ny2, att_names, i
begin
	dims    = dimsizes(Data)
        ny      = dims(0)
        mx      = dims(1)
        ny2     = ny+2

	newData = new((/ny2 ,mx /),typeof(Data))

	newData(1:ny,:) = (/ Data  /)         ; pass values only

        att_names =getvaratts(Data)           ; copy attributes of Data
        if(.not.all(ismissing(att_names)))
                do i = 0,dimsizes(att_names)-1
                        newData@$att_names(i)$ = Data@$att_names(i)$
                end do
        end if
        delete (att_names)

        if (.not.isatt(newData,"_FillValue")) then
            newData@_FillValue = 1.e20        ; arbitrarily assign
        end if

        newData(0,:)     = newData@_FillValue ; bogus end points
        newData(ny2-1,:) = newData@_FillValue
                                              ; copy 1th coordinate info
                                              ; pressure or hgt
        if (.not.ismissing(Data!1) .and. iscoord(Data,Data!1)) then
            newData!1        = Data!1         ; copy the dimension name
            newData&$Data!1$ = Data&$Data!1$  ; copy the coord variables
        end if
                                              ; copy 0th coordinate info
                                              ; latitude here
        if (.not.ismissing(Data!0) .and. iscoord(Data,Data!0)) then
            newData!0 = Data!0                ; copy dimension name
            oldLat    = Data&$Data!0$         ; create a temp 1D array
            newLat    = new ( ny2, typeof(oldLat) )
            newLat(1:ny) = oldLat             ; copy to newLat [no end pts, yet]
            if (newLat(1).lt.0.) then         ; which order are coordinates
                newLat(0)    = -90            ; add an end point
                newLat(ny2-1)=  90            ; add the 2nd end point
            else
                newLat(0)    =  90            ; add an end point
                newLat(ny2-1)= -90            ; add the 2nd end point
            end if

            att_names =getvaratts(oldLat)     ; copy attributes of Lat (if any)
            if(.not.all(ismissing(att_names))) then
                do i = 0,dimsizes(att_names)-1
                        newLat@$att_names(i)$ = oldLat@$att_names(i)$
                end do
            end if
            
            newData&$newData!0$ = newLat
        end if

	return(newData)
end
;*********************************************************
; Dennis Shea    
;
; for x-y plots where x is Months Jan->Dec [size 12]
; create plots with a monthly x labels J,F,M,..,N,D,J
; It will add the cyclic pts

undef("plt_csm_xy_AddCyclicMonth")
function plt_csm_xy_AddCyclicMonth(wks:graphic, x[12]:numeric, y:numeric, resxy:logical)
local nmos, xCyclic,yCyclic,dimy,ranky
begin
  nmos = 12
  xCyclic = ispan(0,nmos,1)  ; 13 values [0,...,12]

  resxy@tmXBMode             = "Explicit"      ; label independently
  resxy@tmXBValues           = xCyclic
 ;if (isatt(resxy,"vpWidthF")) then
 ;    if (resxy@vpWidthF.le.0.7) then
          resxy@tmXBLabels = (/"J","F","M","A","M","J","J","A","S","O","N","D","J"/)
 ;    else
 ;        resxy@tmXBLabels = (/"Jan","Feb","Mar","Apr","May","Jun"      \
 ;                            ,"Jul","Aug","Sep","Oct","Nov","Dec","Jan"/)
 ;    end if
 ;end if

  
  dimy  = dimsizes(y)
  ranky = dimsizes(dimy)

  if (ranky.eq.1) then
      if (dimy(0).ne.12) then
          print("plt_csm_xy_AddCyclicMonth: rightmost y dim must be 12")
          print("plt_csm_xy_AddCyclicMonth: rightmost y="+dimy(0))
          exit
      end if
      yCyclic = new ( (/nmos+1/), typeof(y), getFillValue(y))
      yCyclic(0:nmos-1) = y
      yCyclic(nmos)     = y(0)
  end if

  if (ranky.eq.2) then
      if (dimy(1).ne.12) then
          print("plt_csm_xy_AddCyclicMonth: rightmost y dim must be 12")
          print("plt_csm_xy_AddCyclicMonth: rightmost y="+dimy(1))
          exit
      end if
      yCyclic = new ( (/dimy(0),nmos+1/), typeof(y), getFillValue(y))
      yCyclic(:,0:nmos-1) = y
      yCyclic(:,nmos)     = y(:,0)
  end if

  plot = gsn_csm_xy(wks,xCyclic,yCyclic,resxy)
  return(plot)

end


;*********************************************************
; Adam Phillips
;
; Make zero line as thick as tval...
;
; plot = SetZeroLineThickness(plot,3.5)

undef("SetZeroLineThickness")
function SetZeroLineThickness(plot:graphic,tval:float) 

local cnlvls, cnlinethk, n, N
begin
       cnlvls = get_cnLevels (plot)

       N = dimsizes(cnlvls)
       if (ismissing(N) .or. N.le.0) then
           print ("ZeroLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)               ; no change
       else 
           cnlvls = FixZeroContour (cnlvls, "ZeroLineContour")
       end if

       cnlinethk = new (N, float)            ; create vector for line patterns
       cnlinethk = 1                         ; default

       do n=0,N-1
          if (cnlvls(n).ge.-1.e-05.and.cnlvls(n).le.1.e-05) then
              cnlinethk(n) = tval            ; make the zero contour thicker
          end if
       end do

       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnMonoLineThickness"    : False
           "cnLineThicknesses"      : cnlinethk
       end setvalues

      return (plot)
end
;************************************************************************
; Adam Phillips
;
; Set neg contour lines to dash and draw with the color cneg, 
; draw positive contours with the color cpos, and draw the zero line with 
; the color czero. 

; Usage:  plot= ColorNegDashZeroPosContour(plot,"blue","black","red")
;
;
undef("ColorNegDashZeroPosContour")
function ColorNegDashZeroPosContour(plot:graphic,cneg:string,czero:string,\
                                    cpos:string) 

local cnlvls, cnlinepat, n, N, cncolor
begin

  cnlvls = get_cnLevels (plot)

  N = dimsizes(cnlvls)
  if (ismissing(N) .or. N.le.0) then
     print ("NegDashLineContour: dimsizes(cnlvls)="+N+" return (non-fatal)")
     return (plot)
  else 
     cnlvls = FixZeroContour (cnlvls, "NegDashLineContour")
  end if

  if (any(cnlvls.lt.0.)) then
     cnlinepat = new (dimsizes(cnlvls), integer) ; create array for line pats
     cnlinepat = 0                               ; default is solid (=0)
     cncolor = new (dimsizes(cnlvls), string)    ; create array for line colors
     cncolor = cpos                              ; default is black	  
     do p=0,dimsizes(cnlvls)-1
        if (cnlvls(p).lt.-1.e-05) then
          cnlinepat(p) = 1                       ; simple dash line pattern
	  cncolor(p)   = cneg
        end if
	if (cnlvls(p).ge.-1.e-05.and.cnlvls(p).le.1.e-05) then
	  cncolor(p) = czero
	end if
     end do
    
     ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
     setvalues ovrly_plot
        "cnMonoLineDashPattern"  : False
        "cnLineDashPatterns"     : cnlinepat
        "cnMonoLineColor"        : False
        "cnLineColors"           : cncolor
     end setvalues
   end if                                ; any
   return (plot)
end
;*********************************************************************
; Dennis Shea
; Shades all regions which have a value less then or equal to CnLt
; and all regions which have a value greater than or equal to CnGe
; The pattern and density for each may be different.

undef ("ShadeLeGeContourDensity")
function ShadeLeGeContourDensity (plot:graphic
                          ,CnLt:float, FillPatLt:integer, FillDenLt:float\
                          ,CnGt:float, FillPatGt:integer, FillDenGt:float ) 
; operates on a contour object created by "gsn_csm.ncl" code
; pattern corresponding to "FillPat*" 
;  http://www.ncl.ucar.edu/Document/Graphics/fill_patterns.shtml
;  res@cnMonoFillPattern = False      ; want multiple patterns 
;  res@cnMonoFillScale   = False      ; want patterns w/ diff densities 
;  res@cnFillOn          = True       ; fill contour intervals

local cnlvls, patterns, density, i, N
begin
 
   cnlvls = get_cnLevels (plot)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeLeGeContourDensity: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ShadeLeGeContourDensity")
   end if

   if (any(cnlvls.le.CnLt) .or. any(cnlvls.ge.CnGt)) then
       patterns     = new(dimsizes(cnlvls)+1,integer) ; Create array for fill 
       density      = new(dimsizes(cnlvls)+1,float  )

       density      =  1                           ; default density
       patterns     = -1                           ; patterns and initialize
                                                   ; it to transparent.
       
       do i=0,N-1                        ; Fill contour levels depending on 
         if(cnlvls(i).le.CnLt) then      ; different criteria.
           patterns(i) = FillPatLt       ; see above URL
           density(i)  = FillDenLt       ; see above URL
         end if
          if(cnlvls(i).gt.CnGt) then
             patterns(i) = FillPatGt     ; See above URL
             density(i)  = FillDenGt       ; see above URL
          end if
       end do
       patterns(i)  = FillPatGt          ; last pattern
       density(i)   = FillDenGt          ; last density
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot
           "cnFillOn"          : True
           "cnMonoFillColor"   : True
           "cnMonoFillPattern" : False
           "cnFillPatterns"    : patterns
           "cnFillScales"      : density
       end setvalues
   end if
 
   return (plot)

end
;*********************************************************************
; Adam Phillips
; Shades all regions which have a value less then or equal to nval
; with the color cneg, and shades all regions which have a value greater 
; than or equal to pval with the color cpos.

; usage: ColorShadeLeGeContour(plot,-8.,"blue",8.,"red")

undef("ColorShadeLeGeContour")
function ColorShadeLeGeContour(plot:graphic,nval:float,cneg:string,pval:float,\
                               cpos:string) 

local cnlvls, colist, i, N
begin
 
   cnlvls = get_cnLevels (plot) ; (found in shea_util.ncl)

   N = dimsizes(cnlvls)
   if (ismissing(N) .or. N.le.0) then
       print ("ShadeGeLeContour: dimsizes(cnlvls)=" \
              +N+" return (non-fatal)")
       return (plot)
   else 
       cnlvls = FixZeroContour (cnlvls, "ColorShadeLeGeContour")
   end if

   if (any(cnlvls.ge.pval) .or. any(cnlvls.le.nval)) then
       colist  = new(dimsizes(cnlvls)+1,string)  ; Create array for fill 
       colist(:)  = -1                           ; colors and initialize
                                                 ; it to transparent.       
       do i=0,N-1                                ; Fill contour levels 
         if (cnlvls(i).ge.pval ) then
	    if (cnlvls(i).ge.0. ) then
	       colist(i+1) = cpos
	    else
	       colist(i+1) = cpos
	    end if
	 end if
	 if (cnlvls(i).le.nval) then   
            colist(i) = cneg 
         end if
       end do
     
       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       setvalues ovrly_plot               ; Retrieve contour levels.
           "cnFillOn"          : True
           "cnMonoFillPattern" : True
           "cnMonoFillColor"   : False
           "cnFillColors"      : colist
       end setvalues
   end if
 
   return (plot)

end
undef("upperTriCntr")
function upperTriCntr(wks:graphic,x[*][*]:numeric,label:string,RES:logical)

local res,txres,fs0,fs1,fheight,numl,yoffset,avgx,avgy, \
      tmXTValues, tmYLValues, n, iStr

begin
        fsize = dimsizes(x)
        fs0   = fsize(0)
        fs1   = fsize(1)
        if (fs0.ne.fs1) then
           print("espc: Fatal error: rightmos dimensions must be same") 
           print("espc: dim(0)="+fs0+"  dim(1)="+fs1+"    Exit")           
           exit
        end if

        res = True

        res@cnMissingValPerimOn = True          ; Turn on perimeter around missing data 
        res@cnMissingValPerimThicknessF = 2.0
        res@cnInfoLabelOn   = False             ; turn off contour label

        res@tmXTLabelsOn  = True                ; have tick mark labels
       ;res@tmXTOn        = True                ; have tick marks
        res@tmYROn        = False               ; Turn off right tick marks.
        res@tmXBOn        = False               ; Turn off bottom tick marks.
        res@tmXBBorderOn  = False               ; Turn off bottom border.
        res@tmYRBorderOn  = False               ; Turn off right border.
       ;res@tmEqualizeXYSizes = True            ;Based on font height set in line above, make
                                                ;all tick mark labels the same size
        res@tiYAxisAngleF   = 0.                ;Set rotation in degrees to 0 (default is 90)
        res@tiYAxisString   = "n"               ;Label the Y-axis with "n" (wavenumber)

        if (RES) then
            res = RES
        end if

        plot = gsn_csm_contour(wks,x,res)       ;contour the variable

        getvalues plot
           "tmYLLabelFontHeightF"   : fheight
       ;   "tiYAxisFontHeightF"     : fheight
           "tmXTValues"             : tmXTValues
           "tmYLValues"             : tmYLValues
        end getvalues

        txres = True                            ; Create a new, short resource list for text items
        txres@txFontHeightF = fheight           ; Set the font height
       ;txres@txJust        = "CenterCenter"    ; CenterCenter is the default

;------------Determining the number of labels to show on diagonal axis
        numl = dimsizes(tmXTValues)

        avgx = avg(tmXTValues)                  ; should be same 
        avgy = avg(tmYLValues) 
                                                ; this group of if
        yoffset = 0.5                           ; statement is crude.
        if (fs0.ge.22 .and. fs0.le.51) then     ; BUT easier than computing
            yoffset = 1.5                       ; actual offset
        end if
        if (fs0.ge.52 .and. fs0.le.71) then
            yoffset = 2.5
        end if
        if (fs0.ge.72 .and. fs0.le.99) then
            yoffset = 3.5
        end if
        if (fs0.ge.100) then
            yoffset = 5.5
        end if

        iStr = " "                              ; do this to avoid annoying message
        do n=1,numl-2                           ; numeric labs  along diagonal
           iStr = floattointeger(tmXTValues(n))
           text = gsn_add_text(wks,plot, iStr \
                     ,tmXTValues(n), tmYLValues(n)-yoffset ,txres)
        end do 
                                                ; plot "m" in middle
        text = gsn_add_text(wks,plot, "m"     \
                     ,1.20*tmXTValues(numl/2), 0.90*tmYLValues(numl/2) ,txres)

        text = gsn_add_text(wks,plot, label   \
                     ,0.75*tmXTValues(numl-1), 0.35*tmYLValues(numl-1) ,txres)

        return(plot)
end
; ----------------------------------------------------------------
undef("drawNDCGrid")
procedure drawNDCGrid(wks:graphic) 
;
; Created by Adam Phillips
; operates on a plot object created by "gsn_csm.ncl" code
;
; Draws NDC grid lines at .1 ndc coordinate intervals, and labels them. 
; Used for easier placement of lables/lines on plots.
; Frame must be called after using this procedure. 
; 
;          
; Example: drawNDCGrid(wks)
;       
local colors, nu, numb, cmap, x6res, gh
begin
;; No longer need to add "gray" in V6.1.0 and later
;;        colors = gsn_retrieve_colormap(wks)	;Add gray color to color map
;;        nu = dimsizes(colors)
;;        numb = nu(0)
;;        cmap = new ((/numb+1,3/),float)
;;        cmap(:numb-1,:) = colors
;;        cmap(numb,:) = (/.72,.72,.72/)
;;        gsn_define_colormap(wks,cmap)
;;	delete(numb)
;;	delete(nu)
;;	delete(cmap)
;;	delete(colors)

     	x6res = True                      ; polyline mods desired
        x6res@gsLineColor = "gray"        ; color of lines
        x6res@gsLineThicknessF = 1.5      ; thickness of lines
	x6res@gsLineDashPattern = 1	  ;dash the line
	x6res@gsLineLabelFontColor = "gray"
	x6res@gsLineLabelFontHeightF = 0.0105

	do gh = 1,9
           x6res@gsLineLabelString= gh*10/100.  ; adds a line label string
	   gsn_polyline_ndc(wks,(/0.,1./),(/gh*10/100.,gh*10/100./),x6res)
	   gsn_polyline_ndc(wks,(/gh*10/100.,gh*10/100./),(/0.,1./),x6res)
	end do
	x6res@gsLineLabelString=.01
	gsn_polyline_ndc(wks,(/0.,1./),(/0.01,0.01/),x6res)
	x6res@gsLineLabelString=.99
	gsn_polyline_ndc(wks,(/0.,1./),(/0.99,0.99/),x6res)
	x6res@gsLineLabelString=.01
	gsn_polyline_ndc(wks,(/0.01,0.01/),(/0.,1./),x6res)
	x6res@gsLineLabelString=.99
	gsn_polyline_ndc(wks,(/0.99,0.99/),(/0.,1./),x6res)
end
; -------------------------------------------------------
undef("setColorContourClear")
function setColorContourClear (plot:graphic, cLow:numeric, cUp:numeric) 
; operates on a plot object created by "gsn_csm.ncl" code
; Make color contours between cLow and cUp clear
; good for difference plots
local cnlvls, cncnFillColors, n, N
begin
       cnlvls = get_cnLevels (plot)
      ;print("setColorContourClear:  cnlvls="+cnlvls)

       N = dimsizes(cnlvls)
       if (ismissing(N) .or. N.le.0) then
           print ("ZeroLineContour: dimsizes(cnlvls)=" \
                  +N+" return (non-fatal)")
           return (plot)               ; no change
       else 
           cnlvls = FixZeroContour (cnlvls, "ZeroLineContour")
       end if

       ovrly_plot = GetOverlayPlot (plot, "contourPlotClass", 0)
       getvalues ovrly_plot
         "cnFillColors"   : cnFillColors
       end getvalues

      ;print("setColorContourClear:  cnFillColors="+cnFillColors)

       do n=0,N-1
          if (cnlvls(n).gt.cLow .and. cnlvls(n).le.cUp) then
              cnFillColors(n) = -1
          end if
       end do

       setvalues ovrly_plot
           "cnFillColors"    : cnFillColors
       end setvalues

      return (plot)
end

;-------------------------------------------------------------------
; Mary Haley
;
; This function adds the cone of influence (returned from the wavelet
; function) as a shaded polygon. This function checks whether we're 
; doing both sides or just the right or left side.
;
;     plot:  the plot to add the polygon to
;        w:  the variable (returned from wavelet), must contain
;            "coi" attribute.
;     time:  the time values corresponding with the coi values
;     gsres: optional list of GraphicStyle resources (for polygon fill).
;
undef("ShadeCOI")
function ShadeCOI(wks:graphic,plot:graphic,w,time, gsres:logical)
local newtime, newcoi, ni, indices, do_both, gsres2
begin
  gsres2 = gsres    ; Make a copy b/c we have to make changes to gsres.
;
; Error checking.
;
  if(.not.isatt(w,"coi")) then
    print("ShadeCOI: 'w' must contain an attribute 'coi' that contains the cone of influence")
    return(plot)
  end if

  if(dimsizes(w@coi).ne.dimsizes(time)) then
    print("ShadeCOI: the length of 'w@coi' and 'time' must be the same")
    return(plot)
  end if
;
; Set up some polygon resources.
;
  set_attr(gsres2,"gsFillIndex",5)
  set_attr(gsres2,"gsEdgeThicknessF",3.)
  set_attr(gsres2,"gsEdgesOn",True)

;
; Get the limits of the plot.
;
  getvalues plot
    "trXMinF" : xmin
    "trXMaxF" : xmax
    "trYMinF" : ymin
    "trYMaxF" : ymax
  end getvalues

;
; Get area where shading is to occur.
;
  indices = ind((w@coi.gt.ymin.and.w@coi.lt.ymax).and. \
                (xmin.le.time.and.time.le.xmax))

  if(any(ismissing(indices))) then
    print("ShadeCOI: couldn't find any COI values within range")
    return(plot)
  end if
  
;
; Figure out if both parts of COI are present, or just one.
;
  ni  = dimsizes(indices)
  if(any((indices(1:ni-1)-indices(0:ni-2)).gt.1)) then
    do_both = True
  else
    do_both = False
  end if
  
  if(do_both)
;
; Create some arrays to hold the polygon.
;
    newtime = new(ni+7,float)
    newcoi  = new(ni+7,float)

    newtime(0)      = xmin
    newtime(1)      = tofloat(time(indices(0)))
    newtime(2:ni+1) = tofloat(time(indices))
    newtime(ni+2)   = tofloat(time(indices(dimsizes(indices)-1)))
    newtime(ni+3)   = xmax
    newtime(ni+4)   = xmax
    newtime(ni+5)   = xmin
    newtime(ni+6)   = xmin

    newcoi(0)      = ymin
    newcoi(1)      = ymin
    newcoi(2:ni+1) = tofloat(w@coi(indices))
    newcoi(ni+2)   = ymin
    newcoi(ni+3)   = ymin
    newcoi(ni+4)   = ymax
    newcoi(ni+5)   = ymax
    newcoi(ni+6)   = ymin
  else
;
; Create some arrays to hold the polygon.
;
    newtime = new(ni+6,float)
    newcoi  = new(ni+6,float)
;
; Figure out whether coi values are increasing or decreasing.
;
    if(w@coi(indices(0)).lt.w@coi(indices(ni-1))) then
      increasing = True
    else
      increasing = False
    end if

    if(increasing) then
      newtime(0)      = xmin
      newtime(1)      = tofloat(time(indices(0)))
      newtime(2:ni+1) = tofloat(time(indices))
      newtime(ni+2)   = xmax
      newtime(ni+3)   = xmax
      newtime(ni+4)   = xmin
      newtime(ni+5)   = xmin
  
      newcoi(0)       = ymin
      newcoi(1)       = ymin
      newcoi(2:ni+1)  = tofloat(w@coi(indices))
      newcoi(ni+2)    = tofloat(w@coi(indices(ni-1)))
      newcoi(ni+3)    = ymax
      newcoi(ni+4)    = ymax
      newcoi(ni+5)    = ymin
    else
      newtime(0)      = xmin
      newtime(1)      = xmin
      newtime(2:ni+1) = tofloat(time(indices))
      newtime(ni+2)   = tofloat(time(indices(ni-1)))
      newtime(ni+3)   = xmax
      newtime(ni+4)   = xmax
      newtime(ni+5)   = xmin
  
      newcoi(0)       = ymax
      newcoi(1)       = tofloat(w@coi(indices(0)))
      newcoi(2:ni+1)  = tofloat(w@coi(indices))
      newcoi(ni+2)    = ymin
      newcoi(ni+3)    = ymin
      newcoi(ni+4)    = ymax
      newcoi(ni+5)    = ymax
    end if
  end if
;
; Add cone of influence polygon.
;
  coi           = gsn_add_polygon(wks,plot,newtime,newcoi,gsres2)
  newstr        = unique_string("coi")
  plot@$newstr$ = coi

  delete(newtime)
  delete(newcoi)

  return(plot)
end

undef ("histX")
function histX (x[*]:numeric)             ; 13 Sept 2003
; create an x-axis that will form a coordinate pair with histY
; the purpose is to create a curve that has histogram characteristics
; MUST be used with "histY"
local npt, N, N1, N2, N3, k, n, xNew
begin
  npt = dimsizes(x)   
  N   = 2*npt        
  N1  = N-1        
  N2  = N-2       
  N3  = N-3      

  k         = 0 
  xNew      = new ( N , float)
  xNew(0)   = x(0)
  do n=1,N3,2
     k = k+1
     xNew(n:n+1) = (x(k-1)+x(k))*0.5
  end do
  xNew(N1)  = x(npt-1)

  return(xNew)
end

undef ("histY")
function histY (y[*]:numeric)             ; 13 Sept 2003
; create a y-axis that will form a coordinate pair with histX
; the purpose is to create a curve that has histogram characteristics
; MUST be used with "histX"
local npt, N, N1, N2, N3, k, n, yNew
begin
  npt = dimsizes(y)
  N   = 2*npt
  N1  = N-1    
  N2  = N-2   
  N3  = N-3  

  k         = -1
  yNew      = new ( N , float)
  do n=0,N2,2
     k = k+1
     yNew(n:n+1) = y(k)
  end do
  yNew(N1)   = y(npt-1)

  return(yNew)
end
; ----------------------------------------------------------
; Adam Phillips
;
undef("TimeSourceStamp")
procedure TimeSourceStamp (wks:graphic, chSize:float, Data:string )

; Place text on plot to indicate the data source, program, and 
; time created. This procedure must be invoked prior to any advance
; frame call. Modified from Dennis Shea's procedure infoTimeStamp.
; The program name is found automatically, no need to enter name.
; If the user chooses not to include a data source (just enters ""),
; the data source line is not plotted. Works for both landscaped
; and portrait plots, as well as maximized plots.
;
; examples:
;          (0) wks =  gsn_open_wks("ncgm","bogus") 
;          (1) TimeSourceStamp (wks, 0.01, "slp.mon.mean.nc")
;              [plot]
; txJust: http://www.ncl.ucar.edu/Document/Graphics/Resources/tx.shtml#txJust

local yBot, xLeft, xRight, xoff_set, yoff_set,txres, TimeStamp, script_name \
    , ps_xLeft, ps_yLeft, ps_xRight, ps_orient
begin
   yBot                = 0.001
   xLeft               = 0.001
   xRight              = 0.999
                                        ; psWorkStationClass for NCL v6.0.0 and earlier, documentWorkstationClass for v6.1.0 and newer
   if (NhlClassName(wks).eq."psWorkstationClass".or.NhlClassName(wks).eq."documentWorkstationClass") then 
      getvalues wks
         "wkDeviceLowerX"   : ps_xLeft
         "wkDeviceLowerY"   : ps_yLeft
         "wkDeviceUpperX"   : ps_xRight
	 "wkOrientation"    : ps_orient
      end getvalues
      
      if(ps_xLeft .lt. 0)then           ; 36 is default, 0 is the beginning
         xoff_set = fabs(ps_xLeft/612.) ; 612 is 8.5 * 72 in-1
         xLeft = 0.02941 + xoff_set     ; .02941 is 1/4 of an inch
      else                              ; which is the margin required
         xLeft = 0.02941                ; when printing postscript 
      end if

      if(ps_xRight .gt. 612)then              ; 576 is default,612 is end
         xoff_set = fabs(1 - (ps_xRight/612.)); 612 is 8.5 * 72 in-1
         xRight= 0.97059 - xoff_set
      else
         xRight = 0.97059
      end if

      if(ps_yLeft .lt. 0)then         ; 126 is default, 0 is the beginning
         yoff_set = fabs(ps_yLeft/792.) ; 792 is 11  * 72 in-1
         yBot  = 0.02941  + yoff_set
      else
         yBot  = 0.02941
      end if
      
      if (ps_orient.eq.6.and.xLeft.ne.0.02941) then	
         yBot = yBot+.1		;exception for landscaped plots
  	 xLeft  = xLeft - .08	
	 xRight = xRight+ .08	
      end if
;      print("yBot = "+yBot+", xLeft = "+xLeft+", xRight = "+xRight)
   end if
 ;;script_name = systemfunc("pwd")+"/"+get_script_name()+".ncl"
   script_name = systemfunc("pwd")+"/"+get_script_name()        ; 5.0.1

   txres               = True         ; additional info
   txres@txFontHeightF = chSize       ; size of meta data
   txres@txJust        = "BottomRight"
   if (Data.ne."") then
      gsn_text_ndc  (wks, "Data Source: "+Data ,xRight,yBot + .022 ,txres)
   end if
   if (ismissing(script_name)) then
      gsn_text_ndc  (wks, "Program: Not retrievable" , xRight , yBot, txres)
   else
      gsn_text_ndc  (wks, "Program: "+script_name , xRight , yBot, txres)
   end if

   TimeStamp           = systemfunc( "date" )
   txres@txJust        = "BottomLeft"
   gsn_text_ndc  (wks,"Created: "+TimeStamp , xLeft, yBot, txres)
end
; ----------------------------------------------------------
; Adam Phillips
;
undef("landsea_mask")
function landsea_mask (basemap[*][*]:byte,tlat:numeric,tlon:numeric)
;
;	Given a latitude and longitude array, this function will 
;	return a field that contains a land sea mask. It is up to
;	the user to apply the mask to his/her data. The data contains
;	five types of values: 0=ocean, 1=land, 2=lake, 3=small island, 4=ice shelf
;
;	If the user is not happy with the default data file, the user may modify it 
;	for their own use. Note! This function expects the input array to be of type byte.
;
;	2D lat/lon fields are accepted. integer/float/doubles are accepted
;	longitude arrays accepted from -180:359.999, if your longitudes
;	are < -180, use the contributed function lonFlip to convert
;	your longitudes to 0->360...
;
;	For all four examples:
;	1.  It is assumed array@_FillValue is set for use in the ncl mask function. 
;	2.  The data file must be loaded prior to the function call. The data
;	    file is located in: $NCARG_ROOT/lib/ncarg/data/cdf
;	    To read in the data file: 
;	       data_d = ncargpath("data") + "/cdf/landsea.nc"
;    	       f        = addfile(data_d,"r")
;	       bmap  = f->LSMASK
;
;	    The data file may also be downloaded from the ncl web site at:
;	    http://www.ncl.ucar.edu/Document/Functions/list_func_doc.shtml
;	    and read into your script. 
;	3.  "bmap" is the name of the data array that contains the original land-sea mask.
;	    (see 2. above)
;
;	Example 1 array is a 3D array w/coord vars (time,lat,lon)
;		  Land,lakes, small islands, and ice shelves will be masked.
;		  lsm = landsea_mask(bmap,array&lat,array&lon)
;	          array = mask(array,conform(array,lsm,(/1,2/)).ge.1,False)
;		
;	Example 2 array is a 2D array w/coord vars (lat,lon), land/small islands
;		  will be masked, but not lakes
;		  lsm = landsea_mask(bmap,array&lat,array&lon)	  
;	          array = mask(array,lsm.eq.1,False)
;		  array = mask(array,lsm.eq.3,False)
;	
;	Example 3 array is a 2D array w/2D coordinate variables TLAT/TLON
;		  Land,lakes, small islands, and ice shelves will be masked.
;		  lsm = landsea_mask(bmap,TLAT,TLON)
;		  array = mask(array,lsm.ge.1,False)	
;
;	Example 4 single point: lat1 and lon1 represent a single point. 
;	          lsm = landsea_mask(bmap,lat1,lon1)
;	          print("Value returned = "+lsm)
;	
begin	

        rank = dimsizes(dimsizes(tlon))         ; # of dimensions

	if (rank.eq.2) then
	   ulon = ndtooned(tlon)		;convert 2D lat/lon coords to 1D
	   ulat = ndtooned(tlat)
	   if (any(dimsizes(ulat).ne.dimsizes(ulon))) then
	      print("landsea_mask:")
	      print("Fatal error: 2D lat/lon arrays are different sizes, exiting")
	      exit
	   end if
	   rdim = 1
	else
	   ulon = tlon
	   ulat = tlat  	   
	   rdim = 0
	end if

	ulat = ulat + 90			;add 90 to convert to index
	if (min(ulon).lt.0) then
	   ind_lt0 = ind(ulon.lt.0)
	   ulon(ind_lt0) = ulon(ind_lt0)+360	;add 360 to those lons < 0
	   delete(ind_lt0)
	end if
	
	if (typeof(ulon).eq."integer") then		
	   inlon = ulon
	end if
	if (typeof(ulat).eq."integer") then
           inlat = ulat
	end if
	if (typeof(ulon).eq."float") then        ;lop off decimal pt. 
	   inlon = floattointeger(ulon)		 ;I want 56.0->56.99999 to refer to data
	end if					 ;pt at 56.5
	if (typeof(ulat).eq."float") then		
	   inlat = floattointeger(ulat)			
	end if
	if (typeof(ulon).eq."double") then
	   inlon = doubletointeger(ulon)
	end if	
	if (typeof(ulat).eq."double") then
	   inlat = doubletointeger(ulat)
	end if	
	
	if (min(inlat).lt.0) then
	   ind_lt0 = ind(inlat.lt.0)		;correction for all indices < 0 and > 179
	   inlat(ind_lt0) = 0
	   delete(ind_lt0)
	end if

	if (max(inlat).gt.179) then
	   ind_gt179 = ind(inlat.gt.179)
	   inlat(ind_gt179) = 179
	   delete(ind_gt179)
	end if

	if (max(inlon).ge.360) then
	   if (max(inlon).ge.361) then
	      print("Fatal error: invalid longitude point > 360.999, outside valid longitude range")
	      print("Exiting")
	      exit
	   end if
	   ind_ge360 = ind(inlon.ge.360.) 
	   inlon(ind_ge360) = 0
	   delete(ind_ge360)
	end if
	
	if (rdim.eq.0) then
	   if (dimsizes(inlat).eq.1.and.dimsizes(inlon).eq.1) then		;user passed in 1 pt
	      newarr = basemap(inlat,inlon)
	   else
              ninlat = dimsizes(inlat)
              ninlon = dimsizes(inlon)
	      newarr = new((/ninlat,ninlon/),"byte")
	      newarr!0   = "lat"
	      newarr&lat = tlat
	      newarr!1   = "lon"
	      newarr&lon = tlon
	      do ss = 0,ninlat-1
	         newarr(ss,: ) = (/ basemap(inlat(ss),inlon(:)) /) 
	      end do
	   end if
	end if

	if (rdim.eq.1) then
	   tarr = new((/dimsizes(inlat)/),"byte")
	   
	  ;wcStrt = systemfunc("date")
	   do ss = 0,dimsizes(inlat)-1
	      tarr(ss) = (/ basemap(inlat(ss),inlon(ss)) /)
	   end do
	  ;wallClockElapseTime(wcStrt, "ss do loop", 0)
	   dimt = dimsizes(tlat)
	   newarr = onedtond(tarr,(/dimt(0),dimt(1)/))
	   newarr@lat2d = tlat
	   newarr@lon2d = tlon
	   delete(tarr)
	end if
	return(newarr)	
end
; ----------------------------------------------------------
; Adam Phillips
;
undef("boxplot")
function boxplot (wks:graphic,x[*]:numeric,y[*][*]:numeric,boxOpts:logical,plotres:logical,lineres:logical)
;
;    This function creates a blank linLog plot object, on which box plots are created by extensive
;    use of gsn_add_polyline. The user can draw as many or as few box plots as they wish.
;    The plot is not drawn, and the frame is not advanced. May be used with gsn_panel. Missing data
; may be present in the input Y array, but not the input X array.
;    The function options are as follows:
;
;    wks     ----- The workstation
;    x[*]    ----- A one-dimensional array containing the X-axis values of where the box plot(s)
;                  will be drawn.
;    y[*][*] ----- A two-dimensional array, where the rightmost dimension contains the box plot
;               reference pt. data. y(n,0)=bottom_value, y(n,1)=bottom_value_of_box,
;               y(n,2)=mid-value_of_box,y(n,3)=top_value_of_box,y(n,4)=top_value
;    boxOpts ----- 2 options attached as attributes may be attached here.
;               boxWidth ---- Scalar or array that contains the widths of the boxes.
;               boxColors ---- Scalar or array that contains the colors that the boxes will be drawn in.
;                           Ex. boxOpts@boxColors =  (/"green","blue"/)
;                        If the number of colors specified does not equal the number of
;                        boxes drawn, only the first color specified will be used.
;    plotres ----- An optional xy-plot resource list. Will override defaults where applicable.
;    lineres ----- An optional resource list pertaining to the lines used to draw the boxes/lines.
;
;    Example call: plot3 = boxplot(wks,ntime,newyval,opti,res,False)
;               draw(plot3)
;               frame(wks)
;
begin
     dimquery = dimsizes(y)
     numbox = dimquery(0)
     boxWidths = new((/numbox/),float)
     if (numbox.ne.dimsizes(x)) then
        print("boxplot: Fatal: X must be one-dimensional and both X and Y must have the same leftmost dimension")
        exit
     end if
     if (any(ismissing(x))) then
        print("boxplot: Fatal: X array cannot contain missing data, exiting")
        exit
     end if

;    Developing x-axis
     xAxis = new(numbox+2,typeof(x))
     xAxis(1:numbox) = x
     if (numbox.ne.1) then
        dx = x(1)-x(0)
        xAxis(0) = x(0)-dx
        xAxis(numbox+1) = x(numbox-1)+dx
     else
        dx = 1
        xAxis(0) = x-dx
        xAxis(2) = x+dx
     end if

     if (boxOpts) then
        if (isatt(boxOpts,"boxWidth")) then
           if (dimsizes(boxOpts@boxWidth).ne.1.and.dimsizes(boxOpts@boxWidth).ne.numbox) then
              print("boxplot: Number of input box widths must either equal 1 or the number of boxes ("+numbox+"). Using first specified box width only.")
              boxWidths(:) = boxOpts@boxWidth(0)
           else
              boxWidths = boxOpts@boxWidth
           end if
        else
           boxWidths(:) = dx*.3
        end if
     else
        boxWidths(:) = dx*.3
     end if


     labarr = new(numbox+2,"string")         ;Prepare actual X-axis labels...
     labarr(0) = ""
     labarr(numbox+1) = ""
     labarr(1:numbox) = xAxis(1:numbox)

; Whether to maximize plot in frame.
     maximize = get_res_value(plotres,"gsnMaximize",False)

     if (plotres) then
;       print("Plot resources detected, accepting")
        fsatts = getvaratts(plotres)
        do ty = 0,dimsizes(fsatts)-1
           if (fsatts(ty).eq."tmXBLabels") then        ;Special section to test for XBLabels
              if (dimsizes(plotres@tmXBLabels).ne.numbox) then
                     print("boxplot: Fatal:Number of XB Labels does not match number of boxes, exiting")
                     exit
                  else
                     labarr(1:numbox) = plotres@$fsatts(ty)$
                  end if
                  xblab = plotres@tmXBLabels
                  delete(plotres@tmXBLabels)      ;Delete so tmXBLabels is not used when all
           end if                       ;atts are assigned below...
        end do
        delete(fsatts)
     end if

     plot = create "plot" logLinPlotClass wks
        "trYMinF" : min(y)-2
        "trYMaxF" : max(y)+2
        "trXMinF" : min(xAxis)
        "trXMaxF" : max(xAxis)
        "pmTickMarkDisplayMode" : "Always"
        "tmXBMode"   : "Explicit"
        "tmXBValues" : xAxis
        "tmXBLabels" : labarr
        "tmYROn"  : False
        "tmXTOn"  : False
        "tmYRBorderOn" : False
        "tmXTBorderOn" : False
        "pmTitleDisplayMode": "Always"                   ; allow titles
        "tiMainOn"     : True
        "tiMainString"   : ""
     end create
     if (plotres) then
        attsetvalues(plot,plotres)
     end if

     polyres = True                          ;Set up defaults
     polyres@gsLineColor       = "black"                     ; color of lines
     polyres@gsLineThicknessF  = 1.5                          ; thickness of lines
     polyres@gsLineDashPattern = 0
     if (lineres) then
        fsatts = getvaratts(lineres)
        do ty = 0,dimsizes(fsatts)-1
           polyres@$fsatts(ty)$ = lineres@$fsatts(ty)$
        end do
     end if

     if (boxOpts) then
        if (isatt(boxOpts,"boxColors")) then
           boxcolor = boxOpts@boxColors
           if (dimsizes(boxcolor).eq.1.or.dimsizes(boxcolor).ne.numbox) then
              if (dimsizes(boxcolor).ne.numbox) then
                 print("boxplot: warning: Number of input colors must either equal 1 or the number of boxes ("+numbox+"). Using first specified color only.")
              end if
              polyres@gsLineColor = boxcolor(0)
              cflag = 1
           else
              cflag = 2
           end if
        else
           cflag =1
        end if
     else
        cflag = 1
     end if

     dum = new((/numbox,9/),graphic)
     do gg = 0,numbox-1
        ff = xAxis(gg+1)
        if (cflag.eq.2) then
            polyres@gsLineColor = boxcolor(gg)
        end if
        yy = (/y(gg,4),y(gg,4)/)
        xx = (/(ff-(boxWidths(gg)/8.)),(ff+(boxWidths(gg)/8.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,0) = gsn_add_polyline(wks,plot,xx,yy,polyres)
    end if

        yy = (/y(gg,3),y(gg,4)/)
        xx = (/ff,ff/)
        polyres@gsLineDashPattern = 1
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,1) = gsn_add_polyline(wks,plot,xx,yy,polyres)
        end if
    polyres@gsLineDashPattern = 0

        yy = (/y(gg,3),y(gg,3)/)
        xx = (/(ff-(boxWidths(gg)/2.)),(ff+(boxWidths(gg)/2.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,2) = gsn_add_polyline(wks,plot,xx,yy,polyres)
    end if

        yy = (/y(gg,1),y(gg,3)/)
        xx = (/(ff-(boxWidths(gg)/2.)),(ff-(boxWidths(gg)/2.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,3) = gsn_add_polyline(wks,plot,xx,yy,polyres)
    end if

        yy = (/y(gg,2),y(gg,2)/)
        xx = (/(ff-(boxWidths(gg)/2.)),(ff+(boxWidths(gg)/2.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,4) = gsn_add_polyline(wks,plot,xx,yy,polyres)
    end if

        yy = (/y(gg,1),y(gg,3)/)
        xx = (/(ff+(boxWidths(gg)/2.)),(ff+(boxWidths(gg)/2.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,5) = gsn_add_polyline(wks,plot,xx,yy,polyres)
    end if

        yy = (/y(gg,1),y(gg,1)/)
        xx = (/(ff-(boxWidths(gg)/2.)),(ff+(boxWidths(gg)/2.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,6) = gsn_add_polyline(wks,plot,xx,yy,polyres)
    end if

        yy = (/y(gg,0),y(gg,1)/)
        xx = (/ff,ff/)
        polyres@gsLineDashPattern = 1
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,7) = gsn_add_polyline(wks,plot,xx,yy,polyres)
        end if
    polyres@gsLineDashPattern = 0

        yy = (/y(gg,0),y(gg,0)/)
        xx = (/(ff-(boxWidths(gg)/8.)),(ff+(boxWidths(gg)/8.))/)
    if (.not.(any(ismissing(xx).or.ismissing(yy)))) then
           dum(gg,8) = gsn_add_polyline(wks,plot,xx,yy,polyres)
        end if
 end do
     dumname = unique_string("dum")
     plot@$dumname$ = dum
     if (isvar("xblab").eq."True") then
        plotres@tmXBLabels = xblab      ;reassign XBLabels for possible future plots
     end if
 if (maximize) then
    mres = True
    mres@gsnDraw  = False
    mres@gsnFrame = False
    maximize_output(wks,mres)
 end if
     return(plot)
end
; ---------------------------------
undef("SPECX_ANAL")
function SPECX_ANAL(X[*]:numeric, iopt[1]:integer, jave[1]:integer, pct[1]:numeric)
;
; created to illustrate the various steps used by the builtin function: specx_anal
;
local x, N, xVari, xVaro, cf, cr, ci, px, spcx, total_area, df, frq
    , wgts, sclVar, sdof, acr, tapcf
begin
   N     = dimsizes(X)
   xVari = variance(X)*(N-1.)/N        ; input variance
  
   if (iopt.ge.0) then                 ; remove mean
       x = X - avg(X)
   else
       x = X
   end if

   if (iopt.ge.1) then
       x = dtrend(x,False)             ; step 1: detrend
   end if
                                       ; step 2: Calculate input sample variance 
   xVaro = variance(x)*(N-1.)/N        ; population variance after pre-whitening

   x     = taper (x,pct,0)             ; step 3: taper the series
                                       ; tapering correction factor
   tapcf = 0.5*(128-93*pct)/(8-5*pct)^2; Bloomfield
  
   cf    = ezfftf(x)                   ; step 4: perform forward FFT
   cr    = cf(0,:)                     ; clarity ... real coef
   ci    = cf(1,:)                     ;             imag coef
   delete( cf )

   px    = cr^2 + ci^2                 ; step 5: periodogram of "x"
  
   wgts         = fspan(1,1,jave)
   wgts(0)      = 0.5                  ; not sure why I did this
   wgts(jave-1) = 0.5
   wgts         = wgts/sum(wgts)       ; normalize sum of wgts to one 

   spcx  = wgt_runave(px,wgts,1)       ; weighted run ave

   delete(px)
  
  ;step 7: normalize the area under the curve [6] to the variance.
  ;       Remember the first and last bandwidths are only 0.5*df
   
   df         = 1./N 
   total_area = (spcx(0) + spcx(N/2-1))*(df/2) + sum(spcx(1:N/2-2))*df
   sclVar     = xVaro/total_area
   spcx       = spcx*sclVar
  
   frq        = fspan(df,0.5,N/2)
  
   sdof       = 2/(tapcf*sum(wgts^2))  ; sum squares of normalized wgts 
                                       ; to calculate dof 
   sdof@bw    = 0.5*sdof*df            ; bw: jones pg 202: murphy+katz
   sdof@xvari = xVari
   sdof@xvaro = xVaro
   sdof@frq   = frq
   sdof@spcx  = spcx
   acr        = esacr(x,1)             ; lag-1 autocorrelation                 
   sdof@xlag1 = acr(1) 
  
   return( sdof )
end
; ----------------------------------------------
undef("SPECXY_ANAL")
function SPECXY_ANAL(X[*]:numeric, Y[*]:numeric   \
                    ,iopt[1]:integer, jave[1]:integer, pct[1]:numeric)
;
; created to illustrate the various steps used by the builtin function: specxy_anal
;
local  x, y, N, xVari, xVaro, yVari, yVaro, wgts, sclVarx, sclVary \
    ,  cf, cr, ci, px, spcx, py, spcy, total_area, df, frq, tapcf  \
    ,  xr, xi, yr, yi, cospc, quspc, coher, phase, sclCQ, acr, sdof
begin
   N  = dimsizes(X)
   if (N.ne.dimsizes(Y)) then
       print("SPECXY_ANAL: x/y size mismatch")
       exit
   end if

   xVari = variance(X)*(N-1.)/N        ; input variance
   yVari = variance(Y)*(N-1.)/N    
 
   if (iopt.ge.0) then                 ; remove mean
       x = X - avg(X)
       y = Y - avg(Y)
   else
       x = X
       y = Y
   end if

   if (iopt.ge.1) then
       x = dtrend(x,False)             ; step 1: detrend
       y = dtrend(y,False)    
   end if
                                       ; Calculate variance 
   xVaro  = variance(x)*(N-1.)/N
   yVaro  = variance(y)*(N-1.)/N

   x  = taper (x, pct, 0)    ; step 2: taper the detrended series
   y  = taper (y, pct, 0)
                             ; tapering correction factor
   tapcf = 0.5*(128- 93*pct)/(8 -5*pct)^2   ; Bloomfield

   cf     = ezfftf(x)                  ; step 4: perform forward FFT
   xr     = cf(0,:)                    ; convenience/clarity
   xi     = cf(1,:)               
   px     = xr^2 + xi^2                ; step 5: periodogram

   cf     = ezfftf(y)                  ; step 4: perform forward FFT
   yr     = cf(0,:)      
   yi     = cf(1,:)               
   py     = yr^2 + yi^2 

   delete(cf)

   cospc = xr*yr + xi*yi               ; cospc periodogram
   quspc = xi*yr - xr*yi               ; quspc periodgram

;step 6: perform a running average over the periodogram values using 
;        either runave or wrunave. This creates the spectrum

   wgts  = fspan(1,1,jave)
   wgts(0)      = 0.5                  ; not sure why I did this
   wgts(jave-1) = 0.5

   wgts  = wgts/sum(wgts)              ; normalize sum of wgts to one 
   spcx  = wgt_runave(px,wgts,1) 
   spcy  = wgt_runave(py,wgts,1) 
   cospc = wgt_runave(cospc, wgts ,1) 
   quspc = wgt_runave(quspc, wgts ,1) 

   phase  = -atan2(quspc, cospc)*57.29578
   coher  = ((cospc^2+quspc^2)/(spcx*spcy))

;step 7: normalize the area under the curve [6] to the input variance.
;       Remember the first and last bandwidths are only 0.5*df
 
   df    = 1./N
   x_total_area = (spcx(0) + spcx(N/2-1))*(df/2) + sum(spcx(1:N/2-2))*df
   y_total_area = (spcy(0) + spcy(N/2-1))*(df/2) + sum(spcy(1:N/2-2))*df

   sclVarx = xVaro/x_total_area
   spcx    = spcx*sclVarx
   sclVary = yVaro/y_total_area
   spcy    = spcy*sclVary

; normalized area

   x_nml_area = (spcx(0) + spcx(N/2-1))*(df/2) + sum(spcx(1:N/2-2))*df
   y_nml_area = (spcy(0) + spcy(N/2-1))*(df/2) + sum(spcy(1:N/2-2))*df

   frq = fspan(df,0.5,N/2)

   sclCQ = sqrt(sclVarx*sclVary)
   cospc =  sclCQ*cospc
   quspc = -sclCQ*quspc                ; match NAG values
  
   sdof       = 2./(tapcf*sum(wgts^2)) ; sum the square of the 
                                       ; normalized wgts to calculate dof 
   sdof@bw    = 0.5*sdof*df            ; see jones pg 202: murphy + katz
   sdof@xvari = xVari
   sdof@xvaro = xVaro
   sdof@yvari = yVari
   sdof@yvaro = yVaro
   sdof@frq   = frq
   sdof@spcx  = spcx
   sdof@spcy  = spcy
   sdof@cospc = cospc
   sdof@quspc = quspc
   sdof@coher = coher
   sdof@phase = phase
   acr        = esacr(x,1)             ; lag-1 autocorrelation                 
   sdof@xlag1 = acr(1) 
   acr        = esacr(y,1)             ; lag-1 autocorrelation                 
   sdof@ylag1 = acr(1) 

   return(sdof)
end
; -----------------------------------
undef("FILWGTS")
procedure FILWGTS (nwgt[*]:integer, ihp[1]:integer \
                  ,fca[1]:numeric , fcb[1]:numeric , sigma[1]:numeric \
                  ,pltType[1]:string, opt[1]:logical)
;;  ---------- Sample Usage --------
;;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl" 
;;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
;;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
;;load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl" 
;;  begin
;;    sigma = 1
;;    nwgt  = (/ 101, 365, 731 /)
;;  
;;    ihp   = 0
;;    fca   = 1./100.
;;    fcb   = -999.   
;;  
;;    FILWGTS (nwgt, ihp ,fca, fcb , sigma, "x11", False)
;;  
;;    ihp  = 2                ; bandpass          
;;    fca  = 1./100.
;;    fcb  = 1./20. 
;;    FILWGTS (nwgt, ihp ,fca, fcb , sigma, "x11", False)
;;  end
begin
  
  fStop = 0.5             ; set to 0.5 except when plot subset wanted
  if (ihp.eq.2) then
      fStop = min( (/2.0*fcb, 0.5/) )        
  end if
  if (ihp.eq.0) then
      fStop = min( (/2.0*fca, 0.5/) )
  end if
  if (isatt(opt,"frqPlotStop")) then
      fStop = opt@frqPlotStop
  end if

  wksType = pltType   ; "x11", "ps", "ncgm", "eps", "pdf"

  printWgts  = False               ; default
  if (isatt(opt,"printWgts") ) then
      printWgts  = opt@printWgts
  end if

  printResponse  = False               ; default
  if (isatt(opt,"printResponse") ) then
      printResponse  = opt@printResponse
  end if

  createFileIeee = False

  createFileAscii= False
  if (isatt(opt,"createFileAscii") ) then
      createFileAscii = opt@createFileAscii
      filDirAscii     = "./"
      if (isatt(opt,"filDirAscii") ) then
          filDirAscii = opt@filDirAscii
      end if
  end if
        
  pltDir  = "./"
  if (isatt(opt,"pltDir") ) then
      pltDir = opt@pltDir
  end if

; -------------- end user input
  Nsig = dimsizes(sigma)

  Nwgt = dimsizes(nwgt)           ; # of wgts to test
  Nr   = 2*max(nwgt)+3            ; max # response pts
  if (ihp.eq.2) then
      Nmax = max((/6,Nr/))        ; max # of pts
  else
      Nmax = Nr
  end if
  
  nCurve = Nwgt+1                ; # of curves
  X      = new ( (/nCurve, Nmax/), float) ; curves to hold data
  Y      = new ( (/nCurve, Nmax/), float) 
  X!0 = "curve"
  Y!0 = "curve"
  X!1 = "freq"
  Y!1 = "response"
  
 do ns=0,Nsig-1
  do n=0,Nwgt-1
     wgt  = filwgts_lancos (nwgt(n), ihp, fca, fcb, sigma(ns) )  
     if (printWgts) then
         print (wgt)
     end if
     if (printResponse) then
         print (wgt@freq+"   "+wgt@resp)
     end if
     
     nPts = 2*nwgt(n)+3
     X(n+1,0:nPts-1) = wgt@freq
     Y(n+1,0:nPts-1) = wgt@resp
     if (printWgts) then
         print ("======= nwgt="+nwgt(n)+" =======")  
         print ("        wgt="+wgt)  
     end if
    ;if (createFileIeee) then
    ;    filo = "wgtsIeee."+nwgt(n)+"_sigma"+sigma(ns) 
    ;    system ("/bin/rm -f "+pltDir+filo)
    ;    fbinrecwrite (pltDir+filo,-1,wgt)
    ;end if
     if (createFileAscii) then
         filo = "wgtsAscii."+nwgt(n)+"_sigma"+sigma(ns) 
         system ("/bin/rm -f "+pltDir+filo)
         asciiwrite (pltDir+filo,sprintf("%10.9f", wgt) )
     end if
     delete (wgt)
  end do

  if (fStop.le.0.0 .or. fStop.gt.0.5) then
      print ("Bad fStop: fStop="+fStop)
      exit
  end if
                            ; ideal
  if (ihp.eq.0)  then       ; LOW PASS
      X(0,0) = 0.0                     
      Y(0,0) = 1.0                     
      X(0,1) = fca                   
      Y(0,1) = 1.0                  
      X(0,2) = fca                   
      Y(0,2) = 0.0                  
      X(0,3) = fStop                 
      Y(0,3) = 0.0                  
      gsnTitle  = "Low Pass"
      gsnLeft   = "fca="+sprintf("%4.3f", fca)
     ;gsnRight  = "pa="+sprintf("%4.1f", 1./fca)
      gsnRight  = oneDtostring(nwgt)
  end if
  if (ihp.eq.1)  then       ; HIGH PASS 
      X(0,0) = 0.0                     
      Y(0,0) = 0.0                     
      X(0,1) = fca                   
      Y(0,1) = 0.0                  
      X(0,2) = fca                   
      Y(0,2) = 1.0                  
      X(0,3) = fStop                 
      Y(0,3) = 1.0                  
      gsnTitle  = "High Pass"
      gsnLeft   = "fca="+sprintf("%4.3f", fca)
     ;gsnRight  = "pa="+sprintf("%4.1f", 1./fca)
      gsnRight  = oneDtostring(nwgt)
  end if
  if (ihp.eq.2)  then       ; BAND-PASS 
      X(0,0) = 0.0                     
      Y(0,0) = 0.0                     
      X(0,1) = fca                   
      Y(0,1) = 0.0                  
      X(0,2) = fca                   
      Y(0,2) = 1.0                  
      X(0,3) = fcb                   
      Y(0,3) = 1.0                  
      X(0,4) = fcb                   
      Y(0,4) = 0.0                  
      X(0,5) = fStop                 
      Y(0,5) = 0.0                  
      bLast  = 1./fca
      bStrt  = 1./fcb
      gsnTitle  = "Band Pass: "+bStrt+"-"+bLast
      gsnLeft   = "fca="+sprintf("%6.5f", fca)+"; fcb="+sprintf("%6.5f", fcb)
     ;gsnRight  = "pa="+sprintf("%4.1f", 1./fca)+"; pb="+sprintf("%4.1f", 1./fcb)
      gsnRight  = oneDtostring(nwgt)
  end if

  X@long_name = "frequency"
  Y@long_name = "response"
  
  xx = (/0.0,fStop/)   ; Create data for a polyline for marking
  yy = (/0.0, 0.0 /)   ; the Y = 0.0 line in each graph.

  if (Nsig.eq.1 .and.sigma(ns).eq.1) then
      wks = gsn_open_wks (wksType, "RESP")
  else
      wks = gsn_open_wks (wksType, "RESP_"+sigma(ns))
  end if

  res = True
  res@trYMinF = -0.1
  res@trYMaxF =  1.1
  res@trXMinF =  0.0
  res@trXMaxF =  fStop
  res@tiMainString    = gsnTitle + ":  sigma ="+sprintf("%3.2f", sigma(ns) )
  res@gsnLeftString   = gsnLeft 
  res@gsnRightString  = gsnRight 

 ;res@xyLineThicknessF    = 2.                    ; choose line thickness
 ;plot  = gsn_csm_xy (wks,X,Y,res)                ; create plot

  res@gsnDraw  = False                            ; don't draw
  res@gsnFrame = False                            ; don't advance frame

 ;indStop = ind(X(0,:).ge.fStop)
 ;print ("fStop="+fStop)
 ;print (indStop)
 ;print (X(0,:))
 ;plot  = gsn_csm_xy (wks,X(0,:indStop),Y(0,:indStop),res)  ; create plot
  plot  = gsn_csm_xy (wks,X(0,:),Y(0,:),res)  ; create plot
           
  resGs = True
  resGs@gsLineThicknessF = 2.0

  do n=1,nCurve-1
     resGs@gsLineDashPattern = n-1                ; dash pattern
     indStop = ind(X(n,:).eq.fStop)
     if (.not.ismissing(indStop)) then
         gsn_polyline(wks,plot,X(n,:indStop),Y(n,:indStop),resGs)  
     else
         N = 2*nwgt(n-1)-1 
         indStop = ind(fStop.gt.X(n,0:N-2) .and. fStop.le.X(n,1:N-1))
         iStop   = indStop(0)
         gsn_polyline(wks,plot,X(n,:iStop),Y(n,:iStop),resGs)  
     end if
     delete(indStop)
  end do

  draw(plot)                                      ; draw
  frame(wks)  
 end do    ; sigma loop
end
; -------------------------------
undef("plt_pdfxy")
function plt_pdfxy(wks[1]:graphic, p[*][*], res[1])
;
; produce a nice plot for the output from 'pdfxy'
;
local plt, dimp, ny,mx, ny2,mx2, PDFXY, yAxis, xAxis
begin
  if (all(ismissing(p))) then
      plt = gsn_csm_contour (wks, p, res)
      return(plt)
  end if

  dimp = dimsizes(p)
  ny   = dimp(0)
  mx   = dimp(1)
  ny2  = ny+2
  mx2  = mx+2

  PDFXY = new( (/ny2,mx2/), typeof(p), getFillValue(p))

  PDFXY(1:ny2-2,1:mx2-2) = (/ p /)

  PDFXY(    0,1:mx2-2) = (/ p(   0,:) /)      ; bottom row
  PDFXY(ny2-1,1:mx2-2) = (/ p(ny-1,:) /)      ; top    row
  PDFXY(1:ny2-2,0    ) = (/ p(   :,0) /)      ; left   column
  PDFXY(1:ny2-2,mx2-2) = (/ p(   :,mx-1) /)   ; right  column

  PDFXY(    0,1:mx2-1) = PDFXY@_FillValue
  PDFXY(ny2-1,1:mx2-1) = PDFXY@_FillValue
  PDFXY(1:ny2-1,0    ) = PDFXY@_FillValue
  PDFXY(1:ny2-1,mx2-1) = PDFXY@_FillValue

  xAxis   = fspan(p@binx_bound_min, p@binx_bound_max, mx2)
  yAxis   = fspan(p@biny_bound_min, p@biny_bound_max, ny2)

  PDFXY!0 = "Y"
  PDFXY!1 = "X"
  PDFXY&X = xAxis
  PDFXY&Y = yAxis
  if (isatt(p,"long_name")) then
      PDFXY@long_name = p@long_name
  end if
  if (isatt(p,"units")) then
      PDFXY@units = p@units
  end if

  plt = gsn_csm_contour (wks, PDFXY, res)
  return( plt )
end
;-------
undef("pdfxy_plt")
function pdfxy_plt(wks[1]:graphic, p[*][*], res[1])
; allow an alternative name for the plt_pdfxy function
local plt
begin
  plt = plt_pdfxy(wks, p, res)
  return( plt)
end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("cdnlp")
function cdnlp(sp[*]:numeric, z:numeric)
; Written by:
; Ufuk Utku Turuncoglu (BE) <u.utku.turuncoglu@be.itu.edu.tr>
begin
  kappa = 0.4
  tol = 0.001
  a = log(z/10.0)/kappa

  cd = sp
  cd = 1.15e-3
  u10 = sp/(1.0+a*sqrt(cd))
  u10o = sp*0.0

  ii = (abs(u10-u10o) .gt. tol)
  do while(any(ii))
    u10o = u10
    cd = 4.9e-4+6.5e-5*u10o
    cd = where(u10o .lt. 10.15385, 1.15e-3, cd)
    u10 = sp/(1.0+a*sqrt(cd))
    ii = (abs(u10-u10o) .gt. tol)
  end do

  opt = True
  opt@cd = cd
  opt@wspd = u10
  return(opt)
end

;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("advect_gradient")
;+++ advect gradient was 'promoted' tp contributed.ncl under 'advect_variable'
;+++ http://www.ncl.ucar.edu/Document/Functions/Contributed/advect_variable.shtml
function advect_gradient (u:numeric, v:numeric, x:numeric \
                         ,gridType[1]:integer, con[1]:numeric \
                         ,long_name:string, units:string, iopt[1]:integer)
;
; linear advection of a scalar quantity: UV.GRADIENT(X)
;                                      : U*(dX/dlon) + V*(dX/dlat)
;
; Requires:
;    (1) grid be global cuz spherical harmonics are used
;    (2) required dimension order ([time,[lev,]]lat,lon)
;    (3) the input grids *must* be ordered South==>North
; 
; Nomenclature:
;   u, v      - zonal and meridional wind components [m/s]
;               rightmost dimensions must be ([...,]lat,lon)
;   x         - scalar quantity to be advected 
;               rightmost dimensions must be ([...,]lat,lon)
;               eg: T, Z, divergence, vorticity, latent energy,..., whatever
;   gridType  - type of grid: 0=gaussian, 1=fixed
;   con       - constant which will multiply the final result
;               usually  con=1
;   long_name - descriptive name (eg: "Temperature Advection" )
;   units     - units of result  (eg: "m-K/s" )
;   iopt      - flag: =0 means input dimension names will be retained
;                     =1 means input dimension names will be renamed
;                      to "time", "lev", "lat", "lon" where appropriate.
;   
; Usage:
;   f = addfile ("foo.nc", "r")
;   u = f->U    ; (time,lev,lat,lon)
;   v = f->V
;   T = f->T
;               ; linear advection of temperature
;   Tadv = advect_gradient(u,v,T,0, 1.0,"linear advection of temperature","m-K/s",0)
;
local dimu, dimv, dimx, ranku, rankv, rankx, ier \
    , x_grad_lon, x_grad_lat, advect
begin
  print("============================================================")
  print("advect_gradient is not supported")
  print("Use advect_variable")
  print("http://www.ncl.ucar.edu/Document/Functions/Contributed/advect_variable.shtml")
  exit
  print("============================================================")
                             ; ERROR CHECKING
  dimu  = dimsizes(u)
  dimv  = dimsizes(v)
  dimx  = dimsizes(x)
  ranku = dimsizes(dimu)
  rankv = dimsizes(dimv)
  rankx = dimsizes(dimx)

  ier   = 0
  if (.not.(gridType.eq.0 .or. gridType.eq.1)) then
      print("advect_gradient: unrecognized gridType: only 0 and 1 allowed")
      print("           gridType="+gridType)
      ier = 1
  end if
  if (.not.(ranku.eq.rankv .and. ranku.eq.rankx)) then
      print("advect_gradient: all input arguments must have the same rank")
      print("           ranku="+ranku)
      print("           rankv="+rankv)
      print("           rankx="+rankx)
      ier = ier + 10
  end if
  if (.not.(all(dimu.eq.dimv) .and. all(dimu.eq.dimx))) then
      print("advect_gradient: all input arguments must have the same dimension sizes")
      print("           ranku="+ranku)
      print("           rankv="+rankv)
      print("           rankx="+rankx)
      ier = ier + 100
  end if
  if (rankx.lt.2) then
      print("advect_gradient: variable rank must be at least 2D: ([time,[lev,]]lat,lon)")
      print("advect_gradient: rank="+rankx+"D")
      ier = ier + 1000
  end if

  dnam  = getvardims(x)             ; dimension names
  ynam  = dnam(rankx-2)             ; latitude dimension name
  if (.not.ismissing(ynam) .and. iscoord(x,ynam)) then
      yord  = isMonotonic(x&$ynam$)
      if (yord.le.0) then
          print("advect_gradient: grid is not in South-to-North order.")
          ier = ier + 10000
      end if
  else
      print("advect_gradient: Warning: No named dimensions. Can not check grid order.")
  end if

  if (ier.ne.0) then
      print("advect_gradient: fatal error(s) encountered: ier="+ier)
      exit
  end if

  x_grad_lon = new( dimx, typeof(x), getFillValue(x) )
  x_grad_lat = new( dimx, typeof(x), getFillValue(x) )
  if (gridType.eq.0) then
      gradsg  (x, x_grad_lon, x_grad_lat)
  end if
  if (gridType.eq.1) then
      gradsf  (x, x_grad_lon, x_grad_lat)
  end if
                                        ; advection
  advect = con*(u*x_grad_lon + v*x_grad_lat)
                                        ; add meta data
  copy_VarCoords(x, advect)
  advect@long_name = long_name
  advect@units     = units

  if (iopt.eq.1) then
      if (rankx.eq.2) then
          advect!0 = "lat"
          advect!1 = "lon"
      end if
      if (rankx.eq.3) then
          advect!0 = "lev"
          advect!1 = "lat"
          advect!2 = "lon"
      end if
      if (rankx.eq.4) then
          advect!0 = "time"
          advect!1 = "lev"
          advect!2 = "lat"
          advect!3 = "lon"
      end if
   end if

  return(advect)
end
;--> original, old interface which actually was a typo
undef("advectGradient")
function advectGradient (u:numeric, v:numeric, x:numeric \
                        ,gridType[1]:integer, con[1]:numeric \
                        ,long_name:string, units:string, iopt[1]:integer)
begin
  return(advect_gradient(u , v, x, gridType, con, long_name, units, iopt) )
end
;--> old interface without typo
undef("AdvectGradient")
function AdvectGradient (u:numeric, v:numeric, x:numeric \
                        ,gridType[1]:integer, con[1]:numeric \
                        ,long_name:string, units:string, iopt[1]:integer)
begin
  return(advect_gradient(u , v, x, gridType, con, long_name, units, iopt) )
end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("thornthwaite_driver")
function thornthwaite_driver(t:numeric, lat:numeric, opt[1]:logical, dim[1]:integer, iounit[2]:integer)
local pet, dymo, dimt, ntim, DYMO
;
; Driver for the 'thornthwaite' (6.3.1) function.
; The 1st 4 arguments are the same as 'thornthwaite'. The additional argument
; is an option that allows assorted input units for the input  't' and the
; return 'pet'. 
; 
; Use of this may result in cleaner more concise code.
begin
   if (any(iounit.lt.0) .or. any(iounit.gt.2)) then               ; iounit error check
       print("thornthwaite_driver: unrecognized iounit argument: iounit="+iounit)
       exit
   end if

   if (iounit(0).eq.0) then                               ; degC
       pet = thornthwaite(t, lat, False, dim)
   else if (iounit(0).eq.1) then                          ; degK
       pet = thornthwaite((t-273.16), lat, False, dim)
   else if (iounit(0).eq.2) then                          ; F
       pet = thornthwaite(((t-32)*0.5555556), lat, False, dim)
   end if        ; F
   end if        ; K 
   end if        ; C  

   pet@long_name = "PET: Thornthwaite"

   if (iounit(1).eq.0) then     
        pet@units = "mm/month"
   else if (iounit(1).eq.1) then
        dymo      = (/31,28,31,30,31,30,31,31,30,31,30,31/)
        dimt      = dimsizes(t)
        ntim      = dimt(dim)
        DYMO      = new( ntim, integer) 
        do nt=0,ntim-1,12
           DYMO(nt:nt+11) = dymo
        end do
        pet       = pet/conform(t,DYMO,dim)
        pet@units = "mm/day"
   else if (iounit(1).eq.2) then
        pet       = pet*0.03937008
        pet@units = "inches/month"
   end if        ; mm/month
   end if        ; mm/day
   end if        ; inches/month

   copy_VarCoords(t, pet)
   return(pet)
end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("vertint")
function vertint (X:numeric, dp:numeric, con[1]:numeric \
                   ,long_name:string, units:string, iopt:integer)
; 
; Perform vertical integration:  dim_sum_n(X*dp,lind) 
; 
; Requirement: X must be in the following order: ([time,],lev,lat,lon)
;
; Nomenclature:
; X         - 3D , 4D or 5D array to be integrated. No missing data allowed
;             dimension order ([[case,]time,],lev,lat,lon)
; dp        - pressure thickness computed by "dpres_hybrid_ccm" or "dpres_plevel"
;             Must be the same size/shape as X
;             No missing data allowed.
; con       - constant used to multiply the value after integration
;             If none, set to 1
; long_name - long name of the resultant integrand 
; units     - units of the resultant integrand 
; iopt      - flag: =0 means input dimension names will be retained
;                   =1 means input dimension names will be renamed
;                      to "time", "lat", "lon" where appropriate.
;
; Usage for hybrid levels
;        f = addfile("....", "r")
;        hyai = f->hyai
;        hybi = f->hybi
;        p0   = f->P0                               ; p0=1000 or p0=100000
;        ps   = f->PS
;    
;        dp   = dpres_hybrid_ccm (ps,p0,hyai,hybi)  ; Pa [kg/(m s2)]
;--------------------
; Usage for pressure levels
;        f   = addfile("....", "r")
;        lev = f->lev  ; (/  1,  2,  3,  5,   7, 10, 20, 30, \   ; hPa 
;                           50, 70,100,150, 200,250,300,400, \
;                          500,600,700,775, 850,925,1000 /)
;                      ; uniys of lev and psfc must match
;        psfc= f->PS   ; PA         (time,lat,lon)
;        lev = lev*100 ; make PA to match psfc       
;        lev@units = "PA"
;
;        ptop= 0       ; integrate 0==>psfc at each grid point
;
;                      ; dp(klev,nlat,mlon) or dp(ntim,klev,nlat,mlon)
;        dp  = dpres_plevel(lev, psfc, ptop, 0)     ; Pa [kg/(m s2)]
;--------------------
; Use the 'dp' from above
;        t    = f->T                                ; (time,lev,lat,lon)
;        cp   = 1004.                               ; J/(K kg)     [ m2/(K s2) ]
;        g    = 9.81                                ; m/s2
;
;        con  = cp/g                                ; constant
;        xvi  = vertint(t, dp, con \
;                        ,"Vertically Integrated Internal Energy" \
;                        ,"kg/s2", 0) 

local dimX, dimDP, rankX, Xdp, lev_dim, VI
begin

  dimX  = dimsizes( X )
  dimDP = dimsizes( dp )
  rankX = dimsizes( dimX )

  if (rankX.le.2 .or. rankX.ge.6) then
      print("vertint: variable rank must be 3D or 4D or 5D")
      print("vertint: rank="+rankX+"D")
      exit
  end if

  if (.not.all(dimDP.eq.dimX) ) then  ; error check
      print("vertint: dimension sizes are not equal")
      print("vertint:  dimX="+dimX)
      print("vertint: dimDP="+dimsizes(dp) )
      exit
  end if

  if (isatt(X,"_FillValue") .and. any(ismissing(X)) ) then  ; error check
      print("vertint: No _FillValue allowed")
      print("vertint: X: nFill="+num(ismissing(X)))
      exit
  end if

  if (isatt(dp,"_FillValue") .and. any(ismissing(dp)) ) then  ; error check
      print("vertint: No _FillValue allowed")
      print("vertint: dp: nFill="+num(ismissing(dp)))
      exit
  end if

  Xdp  = X*dp                                ; [? kg/(m s2)]   (temporary variable)
  copy_VarCoords(X, Xdp)

  if (rankX.eq.3) then                       ;   0   1   2
      lev_dim = 0                            ; (lev,lat,lon) 
  end if
  if (rankX.eq.4) then                       ;   0   1   2  ,3
      lev_dim = 1                            ; (time,lev,lat,lon)
  end if
  if (rankX.eq.5) then                       ;   0    1    2 , 3 , 4
      lev_dim = 2                            ; (case,time,lev,lat,lon)
  end if

  VI = dim_sum_n_Wrap( Xdp, lev_dim )        ; sum vertically [ie integrate]
  if (iopt.eq.1) then
      if (rankX.eq.3) then   
          VI!0 = "lat"
          VI!1 = "lon"
      end if
      if (rankX.eq.4) then   
          VI!0 = "time"
          VI!1 = "lat"
          VI!2 = "lon"
      end if
      if (rankX.eq.5) then   
          VI!0 = "case"
          VI!1 = "time"
          VI!2 = "lat"
          VI!3 = "lon"
      end if
  end if

  VI   = con*VI 
  VI@long_name = long_name
  VI@units     = units

  return( VI )
  end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("IntegrateHybrid")
function IntegrateHybrid (X:numeric, dp:numeric, con[1]:numeric \
                         ,long_name:string, units:string, iopt:integer)
;
; Old interface
; Perform vertical integration over hybrid coordinates.
; 
begin
  return( vertint(X, dp, con, long_name, units, iopt) )  ; contributed.ncl
end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------

undef("IntegratePlevel")
function IntegratePlevel (X:numeric, dp:numeric, con[1]:numeric \
                         ,long_name:string, units:string, iopt:integer)
; 
; Old interface:
; Perform vertical integration over constant pressure coordinates.
; 
begin
  return( vertint(X, dp, con, long_name, units, iopt) )  ; contributed.ncl
end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("PotTemp")
; +++ PotTemp was promoted to contributed.ncl
; +++ http://www.ncl.ucar.edu/Document/Functions/Contributed/pot_temp.shtml
function PotTemp (p:numeric, t:numeric, opt:integer)
;
; Compute potential temperature on constant pressure levels
; Nomenclature
;     p       - pressure levels (Pa)  [1D or 3D or 4D]
;     t       - temperature (K)       [1D or 3D or 4D]          
;                              (lev), (lev,lat,lon) , (time,lev,lat,lon)
;     opt     - options: not used
;
local rank, npr, theta, p0
begin
  rankp  = dimsizes(dimsizes(p)) 
  rankt  = dimsizes(dimsizes(t)) 

  p0     = 100000.                  ; default [units = Pa]
  if (isatt(p,"units") .and. (p@units.eq."mb"   .or. \
                              p@units.eq."MB"   .or. \
                              p@units.eq."millibar"    .or. \
                              p@units.eq."millibars"   .or. \
                              p@units.eq."hPa"  .or. \
                              p@units.eq."HPA"  .or. \
                              p@units.eq."hPA"  .or. \
                              p@units.eq."hpa"  )    ) then
      p0 = 1000.
  end if

  if (rankp.eq.rankt) then
      theta = t*(p0/p)^0.286  
  else
      if (rankp.ne.1 .or. .not.(rankt.eq.3 .or. rankt.eq.4)) then
          print("PotTemp: 1D pressure array expected: rankp="+rankp)
          print("PotTemp: only 3D and 4D arrays allowed for temperature: rankt="+rankt)
          exit
      end if

      if (rankt.eq.3) then
          npr = 0     ; 3D
      else                      
          npr = 1     ; 4D
      end if

      theta = t*(p0/conform(t,p,npr))^0.286  
  end if

  theta@long_name = "Potential temperature"
  theta@units     = "K"
  copy_VarCoords(t, theta)

  return( theta )

end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("StaticStability") 
; +++ StaticStability was promoted to contributed.ncl
; +++ http://www.ncl.ucar.edu/Document/Functions/Contributed/static_stability.shtml
function StaticStability (p:numeric, t:numeric, sopt)
;
; Compute Static Stability on constant pressure levels
; Bluestein: Synoptic-Dynamic Meteorology in Midlatitudes
;            pg 197
;            s = -T*d[log(theta)]/dp = -(T/theta)*d(theta)/dp
;
; Nomenclature
;     p       - pressure levels (Pa)  [1D]
;     t       - temperature (K)       [3D or 4D]          
;                                     (lev,lat,lon) , (time,lev,lat,lon)
;     sopt    - options         
;               sopt=0  use ===>  -(T/theta)*d(theta)/dp
;               sopt=1  use ===>  -T*d[log(theta)]/dp
;               same answer should be returned: mainly used for debug
;
; Note: In a statically stable atmosphere: d(theta)/dp < 0, hence s > 0       
;
local theta, rankth, dNam, wrk
begin
  theta  = PotTemp(p, t, 0)      

  rankp  = dimsizes(dimsizes(p))
  rankt  = dimsizes(dimsizes(t))   ; rankt = rank(theta)

  dNam   = getvardims( t )    ; dimension names

  if (rankp.eq.1 .and. rankt.eq.1) then 
      wrk   = theta
      if (sopt.eq.1) then
          wrk = log( wrk )
      end if
      dthdp = center_finite_diff (wrk,p,False,0)
  end if

  if (rankt.eq.3) then 
      wrk   = theta($dNam(1)$|:, $dNam(2)$|:, $dNam(0)$|:)
      if (rankp.eq.1) then
          if (sopt.eq.1) then
              wrk = log( wrk )
          end if
          wrk   = center_finite_diff (wrk,p,False,0)
      end if
      if (rankp.eq.3) then
          wrkp    = p($dNam(1)$|:, $dNam(2)$|:, $dNam(0)$|:)
          if (sopt.eq.1) then
              wrk = log( wrk )
          end if
          wrk   = center_finite_diff (wrk,wrkp,False,0)
      else
          print("StaticStability: rankt=3: rankp="+rankp+": rankp must be 1 or 3")
          exit
      end if
      dthdp = wrk($dNam(0)$|:, $dNam(1)$|:, $dNam(2)$|:)
  end if
  if (rankt.eq.4) then 
      wrk   = theta($dNam(0)$|:,$dNam(2)$|:, $dNam(3)$|:, $dNam(1)$|:)
      if (rankp.eq.1) then
          if (sopt.eq.1) then
              wrk = log( wrk )
          end if
          wrk   = center_finite_diff (wrk,p,False,0)
      end if
      if (rankp.eq.4) then
          wrkp   = p($dNam(0)$|:,$dNam(2)$|:, $dNam(3)$|:, $dNam(1)$|:)
          if (sopt.eq.1) then
              wrk = log( wrk )
          end if
          wrk   = center_finite_diff (wrk,wrkp,False,0)
      else
          print("StaticStability: rankt=4: rankp="+rankp+": rankp must be 1 or 4")
          exit
      end if
      dthdp = wrk($dNam(0)$|:, $dNam(1)$|:, $dNam(2)$|:, $dNam(3)$|:)
  end if
  delete(wrk)

  if (sopt.eq.0) then
      s = -(t/theta)*dthdp
  end if
  if (sopt.eq.1) then
      s = -t*dthdp                          
  end if

  s@long_name = "static stability"
  s@units     = "K/Pa"       ; or "K-m-s2/kg" 
  copy_VarCoords(t,s)

  return(s)
end
;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("PotVortHybrid")
; +++ PotVortHybrid was promoted to contributed.ncl
; +++ http://www.ncl.ucar.edu/Document/Functions/Contributed/pot_vort_hybrid.shtml
function PotVortHybrid\
            (p:numeric, u:numeric, v:numeric, t:numeric
            ,lat[*]:numeric, gridType[1]:integer, opt:integer)
;
; Compute Isentropic Potential Vorticity on hybrid levels
; CCM Processor User's Guide: May 1994: page B-19
; Original source P Rasch and B Boville
;
; Nomenclature
;     p       - pressure levels (Pa)  [3D or 4D]
;     u       - zonal wind  (m/s)     [3D or 4D]          
;     v       - meridional wind (m/s) [3D or 4D]          
;     t       - temperature (K)       [3D or 4D]          
;     gridType- grid type
;               =0 means gaussian grid
;               =1 means regular or fixed grid
;     lat     - latitudes
;     opt     - options: not used
;
; Note: u,v,t,p MUST:
;    [1] be SOUTH-TO-NORTH 
;    [2] be GLOBAL because spherical harmonics are used
;    [3] have named dimensions because data are reordered
;   
; Usage:
;   f    = addfile ("foo.nc", "r")
;   U    = f->U    ; (time,lev,lat,lon) or (lev,lat,lon)
;   V    = f->V
;   T    = f->T
;   lat  = f->lat
;   hyam = f->hyam
;   hybm = f->hybm
;   p0   = f->P0  ; p0=100000
;   ps   = f->PS  ; Pa
;   p    = pres_hybrid_ccm (ps,p0,hyam,hybm)  ; Pa [kg/(m s2)]
;   copy_VarCoords(U,p)
;
;               ; Potential Vorticity
;   PV = PotVortHybrid(p,u,v,t,lat, 0,  0)
;
; Note: A nice basic discussion of PV may be found at:
; Mid-Latitude Atmospheric Dynamics: A First Course
; Jonathan E. Martin, Wiley 2006,   QC880.M36   , pp276-onward
; 
local ranku, rankv, rankt, rankp, npr, nlt, dthdp  \
    , dudp, dvdp ,theta, W, f, vr, dthdx, dthdy, G, W, rad, pv
begin
                                   ; ERROR CHECK
  ranku  = dimsizes(dimsizes(u)) 
  if (.not.(ranku.eq.3 .or. ranku.eq.4)) then
      print("PotVorticityHybrid: only 3D and 4D arrays allowed: rank="+ranku)
      exit
  end if
  rankv  = dimsizes(dimsizes(v)) 
  rankt  = dimsizes(dimsizes(t)) 
  rankp  = dimsizes(dimsizes(p)) 
  if (.not.(ranku.eq.rankv .and. ranku.eq.rankt .and. ranku.eq.rankp)) then
      print("PotVortHybrid: u, v, t, p must be the same rank: ranku=" \
            +ranku+"  rankv="+rankv+"  rankt="+rankt+"  rankp="+rankp)
      exit
  end if

  if (.not.(gridType.eq.0 .or. gridType.eq.1)) then
      print("PotVortHybrid: unrecognized gridType: only 0 and 1 allowed")
      print("               gridType="+gridType)
      ier = 1
  end if

  if ((lat(1)-lat(0)).le.0) then
      print("PotVortHybrid: data must be in S-N order")
      exit
  end if

  if (ranku.eq.3) then       ; (lev,lat,lon)  => (0,1,2) => (npr,nlt,2)
      npr = 0
      nlt = 1                 
  end if
  if (ranku.eq.4) then       ; (time,lev,lat,lon)  => (0,1,2,3) > (0,npr,nlt,2)
      npr = 1
      nlt = 2                  
  end if

  theta  = t*(100000./p)^0.286  
  theta@long_name = "Potential temperature"
  theta@units     = "K"
  copy_VarCoords(t, theta)
 ;printVarSummary( theta )
 ;printMinMax( theta, True )
                              ; compute VERTICAL (pressure) derivatives
  dthdp = center_finite_diff_n (theta,p,False,0,npr)
  dudp  = center_finite_diff_n (    u,p,False,0,npr)
  dvdp  = center_finite_diff_n (    v,p,False,0,npr)
                              ; compute 
                              ; (1) lat/lon temperature gradients [K/m]
                              ; (2) relative vorticity            [1/s]
  dthdx   = theta             ; create E-W gradient array
  dthdy   = theta             ;        S-N        
 ;dthx@long_name = "longitudinal gradient (derivative)"
 ;dthdy@long_name = "latitudinal gradient (derivative)"
 ;dthdx@units     = "K/m"
 ;dthdy@units     = "K/m"
  if (gridType.eq.0) then
      gradsg (theta, dthdx, dthdy) 
      vr = uv2vrG(u,v)
  end if
  if (gridType.eq.1) then
      gradsf (theta, dthdx, dthdy) 
      vr = uv2vrF(u,v)
  end if

  G   = 9.80665                    ; m/s2 ; gravity at 45 deg lat used by the WMO
  W   = 7.292e-5                   ; (1/s)     ; earth ang rotation
  rad = 4.*atan(1.)/180.
  if (typeof(lat).eq.typeof(vr)) then
      f = 2.*W*sin(lat*rad)          ; (1/s)     ; coriolis parameter
  else 
      f = 2.*W*sin(tofloat(lat)*rad)
  end if

  vr  = vr + conform(vr,f,nlt)     ; absolute vorticity   
                                   ; G used for commonly used units
  pv    = -G*(vr*dthdp - (dthdx*dvdp-dthdy*dudp) )  
  pv@long_name  = "potential vorticity"
  pv@short_name = "PV"
  pv@units      = "K m2/kg/s"      ; common units
  copy_VarCoords(t,pv)

  return(pv)
end

;----------------------------------------------------------------------------
;----------------------- Unsupported: DJS -----------------------------------
;----------------------------------------------------------------------------
undef("PotVortIsobaric")
; +++ PotVortHybrid was promoted to contributed.ncl
; +++ http://www.ncl.ucar.edu/Document/Functions/Contributed/pot_vort_isobaric.shtml
function PotVortIsobaric\
            (p[*]:numeric, u:numeric, v:numeric, t:numeric
            ,lat[*]:numeric, gridType[1]:integer, opt:integer)
;
; Compute Potential Vorticity on constant pressure levels
; Bluestein: Synoptic-Dynamic Meteorology in Midlatitudes
;            pg 264  Eq 4.5.93
;
; Nomenclature
;     p       - pressure levels (Pa)  [1D]
;     u       - zonal wind  (m/s)     [3D or 4D]          
;     v       - meridional wind (m/s) [3D or 4D]          
;     t       - temperature (K)       [3D or 4D]          
;     lat     - latitudes
;     gridType- grid type
;               =0 means gaussian grid
;               =1 means regular or fixed grid
;     opt     - options: not used
;
; Note: u,v,t MUST be SOUTH-TO-NORTH 
;       and must be GLOBAL because spherical harmonics are used
;   
; Usage:
;   f   = addfile ("foo.nc", "r")
;   U   = f->U    ; (time,lev,lat,lon) or (lev,lat,lon)
;   V   = f->V
;   T   = f->T    ; K
;   lat = f->lat
;   lev = f->lev  
;   lev = lev*100
;   lev@units = "Pa"
;               ; Potential Vorticity
;   PV = PotVortIsobaric(lev,u,v,t,lat, 0, 0)
;
; Note: A nice basic discussion of PV may be found at:
; Mid-Latitude Atmospheric Dynamics: A First Course
; Jonathan E. Martin, Wiley 2006,   QC880.M36   , pp276-onward
;
local ranku, rankv, rankt, npr, nlt, dthdp, dudp, dvdp \
     ,theta, s, con, R, W, f, vr, dtdx, dtdy, rad, pv
begin
                                   ; ERROR CHECK
  ranku  = dimsizes(dimsizes(u)) 
  if (.not.(ranku.eq.3 .or. ranku.eq.4)) then
      print("StaticStabilityP: only 3D and 4D arrays allowed: rank="+ranku)
      exit
  end if
  rankv  = dimsizes(dimsizes(v)) 
  rankt  = dimsizes(dimsizes(t)) 
  if (.not.(ranku.eq.rankv .and. ranku.eq.rankt)) then
      print("PotVortIsobaric: u, v, t must be the same rank: ranku=" \
            +ranku+"  rankv="+rankv+"  rankt="+rankt)
      exit
  end if

  if (.not.(gridType.eq.0 .or. gridType.eq.1)) then
      print("PotVortIsobaric: unrecognized gridType: only 0 and 1 allowed")
      print("           gridType="+gridType)
      ier = 1
  end if

  if ((lat(1)-lat(0)).le.0) then
      print("PotVortIsobaric: data must be in S-N order")
      exit
  end if

  if (ranku.eq.3) then       ; (lev,lat,lon)  => (0,1,2) > (0,npr,nlt)
      npr = 0
      nlt = 1
  end if
  if (ranku.eq.4) then       ; (time,lev,lat,lon)  => (0,1,2,3) > (0,npr,nlt,2)
      npr = 1
      nlt = 2
  end if

  theta  = t*(100000./conform(t,p,npr))^0.286  
  theta@long_name = "Potential temperature"
  theta@units     = "K"
  copy_VarCoords(t, theta)
 ;printVarSummary( theta )
 ;printMinMax( theta, True )
                              ; compute VERTICAL (pressure) derivatives
  dthdp = center_finite_diff_n (theta,p,False,0,npr)
  dudp  = center_finite_diff_n (    u,p,False,0,npr)
  dvdp  = center_finite_diff_n (    v,p,False,0,npr)

  s           = -(t/theta)*dthdp
  s@long_name = "static stability"
  s@units     = "K/Pa"       ; or "K-m-s2/kg" 
  copy_VarCoords(t,s)
 ;printVarSummary( s )
 ;printMinMax( s, True )
                              ; compute 
                              ; (1) lat/lon temperature gradients [K/m]
                              ; (2) relative vorticity            [1/s]

  dtdx   = t                  ; create E-W gradient array
  dtdy   = t                  ;        S-N        
 ;dtdx@long_name = "longitudinal gradient (derivative)"
 ;dtdy@long_name = "latitudinal gradient (derivative)"
 ;dtdx@units     = "K/m"
 ;dtdy@units     = "K/m"
  if (gridType.eq.0) then
      gradsg (t, dtdx, dtdy) 
      vr = uv2vrG(u,v)
  end if
  if (gridType.eq.1) then
      gradsf (t, dtdx, dtdy) 
      vr = uv2vrF(u,v)
  end if

  G   = 9.80665                    ; m/s2 ; gravity at 45 deg lat used by the WMO
  R   = 287.04                     ; m2/(s2-K) ; gas constant dry air
  W   = 7.292e-5                   ; (1/s)     ; earth ang rotation
  rad = 4.*atan(1.)/180.
  if (typeof(lat).eq.typeof(vr)) then
      f = 2.*W*sin(lat*rad)          ; (1/s)     ; coriolis parameter
  else
      f = 2.*W*sin(tofloat(lat)*rad)
  end if

  vr  = vr + conform(vr,f,nlt)     ; absolute vorticity   
  con = R/(s*conform(t,p,npr))  
                                   ; G* added to get common pv units 
  pv    = -G*(vr + con*(dvdp*dtdx-dudp*dtdy))*dthdp  
  pv@long_name  = "potential vorticity"
  pv@short_name = "PV"
  pv@units      = "K m2/kg/s"      ; common units
  copy_VarCoords(t,pv)

  return( pv )
end
; ******************************************************************
; *******  THERE IS NO SUPPORT FOR THE PIE CHART FUNCTION **********
; ******************************************************************
; *******              PIE CHART FUNCTION                 **********
; ******************************************************************
undef("pie_chart")
function pie_chart(wks:graphic, percent[*]:numeric, name[*]:string        \
                              , colors[*]:string, pieChartRes:logical)
;                    Create a Pie Chart
; Nomenclature
;      percent     - percent [%: 0-100] for each section: sum(percent)=100
;                    percent = (/ 30, 10.5, 20, 39.5 /)/)
;      name        - names to be associated with each section
;                    name = (/ "Apple", "Spinach", "Orange", "Banana"/)
;      color         colors for each section
;                    color = (/ "red", "green", "blue", "brown" /)
;      pieChartRes - resources which affect plot appearance    
;                    Some examples
;                    pieChartRes = True
;                    pieChartRes@tiMainString= " ..."   [default is no title]
;                    pieChartRes@pcLabelType = "section"   [default]
;                    pieChartRes@pcLabelType = "block"  [labels beneath chart]
 begin
  rad       = 4.*atan(1.0)/180.                ; degress to radians
  nSections = dimsizes(percent) 
                                               ; Specify limits for X and Y axes.
  circ              = 75                       ; arbitrary
  extraSpace        = 100-circ                 ; Extra space beyond outer circle

  opts              = True                     ; local and default options
  if (pieChartRes .and. \
      isatt(pieChartRes,"pcLabelType") .and. pieChartRes@pcLabelType.eq."block") then
                                               ; block
      opts@vpXF     = 0.125                    ; default=0.2
      opts@vpYF     = 0.925                    ; default=0.8 
      opts@vpWidthF = 0.75                     ; default=0.6
      opts@vpHeightF= 0.75                     ; default=0.6
  else                                        
                                               ; section
      opts@vpXF     = 0.125                    ; default=0.2
      opts@vpYF     = 0.90                     ; default=0.8 
      opts@vpWidthF = 0.75                     ; default=0.6
      opts@vpHeightF= 0.75                     ; default=0.6
  end if

  opts@trXMinF      = -circ-extraSpace         ; min X 
  opts@trXMaxF      =  circ+extraSpace         ; max X
  opts@trYMinF      = -circ-extraSpace         ; min Y
  opts@trYMaxF      =  circ+extraSpace         ; max Y

  opts@tmXTOn       = False                    ; turn off tick marks on each side
  opts@tmXBOn       = False
  opts@tmYLOn       = False
  opts@tmYROn       = False
  opts@tmXBBorderOn = False                    ; turn off borders on each side
  opts@tmXTBorderOn = False
  opts@tmYLBorderOn = False
  opts@tmYRBorderOn = False
  opts@gsnFrame     = False                    ; do not advance frame
  opts@gsnDraw      = False                    ; do not draw
  opts@xyMonoDashPattern = True   

  if (pieChartRes .and. isatt(pieChartRes,"tiMainString")) then
      opts@tiMainString   = pieChartRes@tiMainString
      opts@tiMainOffsetYF = -0.05              ; move tiMainString down a bit
                                               ; leave roome for section label
      if (isatt(pieChartRes,"pcLabelType") .and. \
          pieChartRes@pcLabelType.eq."block") then
          opts@tiMainOffsetYF = -0.065         ; no section label, move down more 
      end if
  end if
 ;print(opts)

  plotPC = gsn_xy(wks,(/0.0,0.0/),(/0.0,0.0/),opts)      ; background 
;;plotPC = gsn_csm_xy(wks,(/0.0,0.0/),(/0.0,0.0/),opts)  

  plRes  = True                                ; polyline resources
  plRes@gsLineColor        = "black"
  plRes@gsLineThicknessF   = 1.0

  gsRes  = True                                ; polymarker resources
  gsRes@gsMarkerIndex      = 16
  gsRes@gsMarkerSizeF      = 0.0115  
 ;gsRes@gsMarkerThicknessF = 0.0175
  if (pieChartRes .and. isatt(pieChartRes,"gsMarkerSizeF")) then
      gsRes@gsMarkerSizeF  = pieChartRes@gsMarkerSizeF
  end if

  txRes  = True                                ; text resources
  txRes@txJust             = "CenterLeft"
  txRes@txFontHeightF      = 0.0125            ; default=0.05
  if (pieChartRes .and. isatt(pieChartRes,"txFontHeightF")) then
      txRes@txFontHeightF = pieChartRes@txFontHeightF
  end if

  pgRes  = True                                ; polygon resources

  xOrig  = 0.0
  yOrig  = 0.0
  xLoc   = 0.0
  yLoc   = 0.0

  pcPie  = (percent/100.)*360                  ; percent of 360 degrees 
  pcStrt = 0.0 
  pcLast = pcPie(0)
  phase  = 90.0 - 0.5*pcPie(0)                 ; 'center' 1st section n=0

  do n=0,nSections-1
                                               ; create nodes for section
   ;;npts  = round(pcPie(n), 3)                ; pre 6.4.0
     npts  = max( (/round(pcPie(n),3),2/) )    ; accommodate 6.3.0 fspan change

     npts2 = npts+2
     xx    = new (npts2, "double", "No_FillValue")
     yy    = new (npts2, "double", "No_FillValue")
     xx(0) = xOrig
     yy(0) = yOrig
     xx(1:npts2-2) = circ*cos((fspan(pcStrt, pcLast, npts)+phase)*rad)
     yy(1:npts2-2) = circ*sin((fspan(pcStrt, pcLast, npts)+phase)*rad)
     xx(npts2-1)   = xOrig
     yy(npts2-1)   = yOrig

     pgRes@gsFillColor   =  colors(n)          ; fill with associated color
     plotPC@$unique_string("dum")$ = gsn_add_polygon (wks, plotPC, xx, yy, pgRes)  ; color fill
     plotPC@$unique_string("dum")$ = gsn_add_polyline(wks, plotPC, xx, yy, plRes)  ; outline

     gsRes@gsMarkerColor = colors(n)           ; associate marker
     if (percent(n).lt.10) then
         text = sprintf("%5.1f", percent(n))+"  "+name(n)  ; include %
     else
         text = sprintf("%4.1f", percent(n))+"  "+name(n)  ; include %
     end if
                                               ; labels
  if (pieChartRes .and. \
      isatt(pieChartRes,"pcLabelType") .and. pieChartRes@pcLabelType.eq."block") then
                                               ; block labels
     if ((n%2).eq.0) then
         mx   = -circ+25.                      ; left  column labels    
     else
         mx   =  circ*0.25                     ; right column labels
     end if
   ;;datatondc(plotPC,0.0,-circ, xLoc, yLoc)
   ;;ny = yLoc - ????
     ny   = -(circ+5)
     ix   = 5   
     iy   = 6.0
     jy   = ny-(n/2)*iy
    ;print("jy="+jy)
     plotPC@$unique_string("dum")$ = gsn_add_polymarker(wks, plotPC,       mx   , jy, gsRes)
     plotPC@$unique_string("dum")$ = gsn_add_text      (wks, plotPC, text, mx+ix, jy, txRes)

  else    ; must be "section" labeling
                                               ; section labels
     xxInfo = (circ+0.25*extraSpace)*cos(((pcStrt+pcLast)*0.5+phase)*rad)
     yyInfo = (circ+0.25*extraSpace)*sin(((pcStrt+pcLast)*0.5+phase)*rad)
     angle  = atan2(xxInfo,yyInfo)/rad
    ;print("n="+n+"   angle="+angle+"   "+colors(n))
     ix     = 5   
     if (angle.gt.-0.001) then                 ; round
         txRes@txJust  = "CenterLeft"
         plotPC@$unique_string("dum")$ = gsn_add_polymarker(wks, plotPC,       xxInfo   , yyInfo, gsRes)
         plotPC@$unique_string("dum")$ = gsn_add_text      (wks, plotPC, text, xxInfo+ix, yyInfo, txRes)
     else
         txRes@txJust  = "CenterRight"
         plotPC@$unique_string("dum")$ = gsn_add_polymarker(wks, plotPC,       xxInfo   , yyInfo, gsRes)
         plotPC@$unique_string("dum")$ = gsn_add_text      (wks, plotPC, text, xxInfo-ix, yyInfo, txRes)
     end if

  end if

     delete(xx)
     delete(yy)

     if (n.lt.(nSections-1)) then 
         pcStrt = pcLast+0.001
         pcLast = pcLast+pcPie(n+1)
     end if
  end do

  if (.not.isatt(pieChartRes,"gsnDraw") .or. \
     (isatt(opts,"gsnDraw") .and. opts@gsnDraw)) then 
	draw(plotPC)
  end if

  if (.not.isatt(pieChartRes,"gsnFrame") .or. \
     (isatt(opts,"gsnFrame") .and. opts@gsnFrame)) then 
	frame(wks)
  end if

  return (plotPC)
 end
;-------------------------------------------------------

undef("sstLandFill")
procedure sstLandFill(sst)
; Fill all land with interpolated values 
local dims, ntim,nlat,mlon,sstice,latSH,ni,ml  \
    , opts, optp, nscan, eps, gtype, guess, relc

begin
  dims   = dimsizes(sst)
  ntim   = dims(0)
  nlat   = dims(1)
  mlon   = dims(2)

  sstice = -1.8
  latSH  = -70.    ; all pts polward set to sstice
  ni     = 4       ; max # of pts in longitude direction

;-----------------------------------------------------------------
; Arbitrarily set all grid pts poleward of 70S to sstice
;-----------------------------------------------------------------
  work = conform(sst, sst&lat, 1)
  sst  = where(work.le.latSH, sstice, sst)
  delete(work)     ; not necessary       

;-----------------------------------------------------------------
; Set nearest neighbor values                              
;-----------------------------------------------------------------
  work = sst

  do ml=0,mlon-2   ; left-to-right
     sst(:,:,ml+1) = where(.not.ismissing(work(:,:,ml)) .and. ismissing(work(:,:,ml+1)) \
                          ,work(:,:,ml), work(:,:,ml+1))
  end do

  work = sst       ; use 'updated' sst

  do ml=mlon-1,1,1 ; right-to-left
     sst(:,:,ml-1) = where(.not.ismissing(work(:,:,ml)) .and. ismissing(work(:,:,ml-1)) \
                          ,work(:,:,ml), work(:,:,ml-1))
  end do
  delete(work)     ; not necessary       
  
;-----------------------------------------------------------------
; Linearly interpolate in longitude spannin a max of "ni" pts
; Rationale: SST contours have small variations in longitude
;-----------------------------------------------------------------
  opts  = (/0, ni/)
  sst   = linmsg(sst,opts)

;-----------------------------------------------------------------
; Set the poisson_grid_fill variables
; Global grid: Fill in over land             
;-----------------------------------------------------------------
  nscan = 2000     ; usually *much* fewer
  eps   = 0.001    ; variable depended
  gtype = True     ; Cyclic in longitude [global]
  guess = 0        ; use zonal means
  relc  = 0.6      ; standard relaxation coef
  optp  = 0

  poisson_grid_fill( sst, gtype, guess, nscan, eps, relc, optp)

;-----------------------------------------------------------------
; Make sure no SST are .lt. sstice
;-----------------------------------------------------------------
  sst   = sst > sstice
end

;***********************************************************
; Mimic MATLAB fftshift:
;    http://www.mathworks.com/help/techdoc/ref/fftshift.html 
;***********************************************************
; test code is at: /Users/shea/Data/CarlSchreck/tst_shiftarray.ncl
;***********************************************************

undef ("shift_array")
procedure shift_array(x[*][*], kmode)
local i, j, dimx, nrow, ncol, jwrap, iwrap
begin
  dimx = dimsizes(x)
  nrow = dimx(0)
  ncol = dimx(1)

  tmp  = x
      
  if (kmode.eq.0) then                   ; quadrant shift
      do j=0,ncol-1
         jwrap = mod(j+ncol/2,ncol)
        do i=0,nrow-1
           iwrap = mod(i+nrow/2,nrow)
           tmp(iwrap,jwrap) = x(i,j)
        end do
      end do
  end if

  if (kmode.lt.0) then                   ; column shift 
      do j=0,ncol-1
         jwrap = mod(j+ncol/2,ncol)
         tmp(:,jwrap) = x(:,j)
      end do
  end if
      
  if (kmode.gt.0) then                   ; row shift
      do i=0,nrow-1
         iwrap = mod(i+nrow/2,nrow)
         tmp(iwrap,:) = x(i,:)
      end do
  end if
      
  x = tmp                                ; return to original array
end
;-------------------------------
undef("maxrunlen")
function maxrunlen( runs )                     
; find max run length
; Example:   p(ntim) or p(ntim,jlat,ilon)
; runs   = dim_runnum_n(p.gt.5, 0, 0) ; runs(ntim) or runs(ntim,jlat,ilon)
; maxrun = maxrunlen( runs )

local dimr, rankr, maxrun, i, j, indrun
begin
  dimr   = dimsizes(runs)         
  rankr  = dimsizes(dimr)

  if (.not.(rankr.eq.1 .or. rankr.eq.3)) then
      print("maxrunlen: rankr="+rankr+" not yet supported")
      exit
  end if

  if (rankr.eq.1) then
      maxrun = new( 1, "integer", -999)
      indrun = ind( runs.ne.0 )   
      if (.not.ismissing(indrun(0))) then
          maxrun = indrun(dimsizes(indrun)-1) + 1
      else
          maxrun = 0
      end if
  end if

  if (rankr.eq.3) then
      maxrun = new ( dimr(1:rankr-1), "integer", -999)
      do j=0,dimr(1)-1
        do i=0,dimr(2)-1
           indrun = ind( runs(:,j,i).ne.0 )   
           if (.not.ismissing(indrun(0))) then
               maxrun(j,i) = indrun(dimsizes(indrun)-1) + 1
           end if
           delete(indrun)       ; may change size next iteration
        end do
      end do
      copy_VarCoords(runs(0,:,:), maxrun)
  end if

  maxrun@long_name = "max run length"
  return(maxrun)
end
;-------------------------------
undef("grad_rich_num_atm")
function grad_rich_num_atm(th:numeric, u:numeric, v:numeric ,z:numeric, ndim[1]:integer)
begin
 print("shea_util: function grad_rich_num_atm has been replaced")
 print("Use rigrad_bruntv_atm located in contributed.ncl: 6.3.1")
 exit 
 return("error")
end
;-------------------------------
undef("area_global_rectilinear_grid")
function area_global_rectilinear_grid (lat[*]:numeric, lon[*], opt[1]:logical)
; Area (approximate) of a global rectilinear grid cells.
; Actually, the rectilinear grid must be equally spaced: 1x1, 1x2, 3x5
; This could be modified for non-global grids by adding
;     the left and right and left dx.
local rearth, nlat, mlon, pi, rad, rr, dlon, dx, dy
    , area_lat, area_grid, area_total, area_sphere  
begin
  rearth = 6371.220d0            ; default; kilometers
  if (opt .and. isatt(opt, "rearth")) then
      rearth = opt@rearth 
  end if

  nlat   = dimsizes(lat)
  mlon   = dimsizes(lon)

  pi     = 4d0*atan(1.0d0)
  rad    = pi/180d0
  rr     = rearth*rad
  dlon   = rr*(lon(2)-lon(1))

  dx     = dlon*cos(lat*rad)   ; lon spacing at each latitude

  if (lat(0).lt.-89.9999d0) then ; force exact 0.0, rounding issue
      dx(0)  = 0.0d0
  end if
  if (lat(nlat-1).gt.89.9999d0) then
      dx(nlat-1)  = 0.0d0
  end if
                               ; lat can have variable spacing
  dy           = new (nlat, "double", "No_FillValue")
  dy(0)        = (lat(1)-lat(0))*rr
  dy(1:nlat-2) = (lat(2:)-lat(1:nlat-2))*rr
  dy(nlat-1)   = (lat(nlat-1)-lat(nlat-2))*rr

  area_lat      = dx*dy        ; [*]
  area_grid     = conform_dims( (/nlat,mlon/), area_lat, 0)
  area_total    = sum( area_grid )

  area_sphere   = 4d0*pi*rearth^2

  area_grid@long_name  = "area of each grid cell"
  area_grid@units      = "km^2"
  area_grid@area_total = area_total      ; scalar
  area_grid@area_lat   = area_lat        ; [*]
  area_grid@rearth     = rearth          ; scalar
  area_grid@area_sphere= area_sphere     ; scalar
  area_grid@area_ratio = area_total/area_sphere    ; scalar

  area_grid!0   = "lat"
  area_grid!1   = "lon"
  area_grid&lat =  lat
  area_grid&lon =  lon

  return(area_grid)
end
;-------------------------------
undef("area_polar_region")
function area_polar_region (lat[1]:numeric, opt[1]:logical)
; Area from pole to latitude. Special case of area_sphere_region
local rearth, pi, rad, rr, twopi, parea
begin
  rearth = 6371.220d0            ; default; kilometers
  if (opt .and. isatt(opt, "rearth")) then
      rearth = opt@rearth
  end if

  pi     = 4d0*atan(1.0d0)
  rad    = pi/180d0
  twopi  = 2.0d0*pi
  parea  = twopi*rearth*rearth*(1.0d0-sin(rad*lat))
  parea@long_name = "Polar Area"
  parea@units     = "km^2"

  return(parea)
end
;-------------------------------
undef("area_earth_region")
function area_earth_region (lat1[*]:numeric, lat2[*]:numeric \
                           ,lon1[*]:numeric, lon2[*]:numeric \
                           ,opt[1]:logical)
; sfc area of a region of the globe
; area_g = area_area_earth_region( 90,-90,   0,360,False) ; globe
; area_g = area_area_earth_region( 90,-90,-180,180,False) ; globe
; area_p = area_area_earth_region( 90, 45,   0,360,False) ; pole  area
; area_p = area_area_earth_region(-90,-60,   0,360,False) ; pole  area
; area_r = area_area_earth_region( 30,-30, -45, 60,False) ; region area
local rearth, pi, rad, twopi, lonRatio, con, area
begin
                           ; Radius of sphere with same volume as earth
  rearth = 6371.220d0      ; default; kilometers; ECMWF uses this value
  if (opt .and. isatt(opt, "rearth")) then
      rearth = opt@rearth
  end if

  if (any( (lat2-lat1).gt.180) ) then
      print("area_earth_region: latitude span is incorrect")
      exit
  end if

  if (any( (lon2-lon1).gt.360) ) then
      print("area_earth_region: longitude span is incorrect")
      exit
  end if

  pi    = 4d0*atan(1.0d0)
  rad   = pi/180d0
  twopi = 2.0d0*pi
  lonRatio = abs(lon2-lon1)/360d0
  con   = (twopi*rearth^2)*lonRatio

  area  = con*abs(sin(rad*lat1)-sin(rad*lat2))
  area@long_name = "Area"
  area@units     = "km^2"
  return(area)
end
;---------------------------------------------------------------------
undef("circle_ll")
;
; Author: Arindam Chakraborty <arch at caos.iisc.ernet.in>
;
procedure circle_ll(wks:graphic,plt:graphic,x0:numeric,y0:numeric,r:numeric,res:logical)
; Draws a circle (or ellipse) or radius (or x-axis length) r centered at (x0,y0). Optionally it can draw an ellipse and axes can be rotated as desired.
; Input Parameters:
; wks: graphics workstation
; plt: the plot graphics
; (x0,y0): center of the circle in terms of plot coordinate
; r: radius (or x-direction length) of the circle in terms of plot coordinate
; res: resources. If true can take any line graphic parameter valid to gsn_add_polyline. In addition, there are two special resources for this procedure:
;      Scale: The scale by which the y-axis length should be modified to the x-axis length. This can be used to plot an ellipse. (default=1).
;      Rotation: The rotation angle (in degrees) by which the axes are rotated (positive anticlockwhise). (default=0).
;
; Example:
;...
; arr=random_uniform(1,10,(/15,20/))
; wks = gsn_open_wks("x11","circle");
; res = True
; res@vpHeightF=0.5
; res@vpWidthF=0.8
; res@gsnDraw = False
; res@gsnFrame = False
; plt=gsn_csm_contour(wks,arr,res)

; resc=True
; resc@gsLineColor="red"
; resc@gsLineThicknessF=2
; resc@Scale=1.5
; resc@Rotation=-45
; circle_ll(wks,plt,10,7,3,resc)
; draw(plt)
; frame(wks)
;...

begin
 rot = 0.0
 scale = 1.0
 th = ispan(0,360,1)
 pi = acos(-1.0)

 res1 = res
 if(res)then
  if(isatt(res,"Scale"))then
   scale = res@Scale
   delete(res1@Scale)
  end if
  if(isatt(res,"Rotation"))then
   rot = res@Rotation
   delete(res1@Rotation)
  end if
 end if

 getvalues plt
  "trXMinF":xmin
  "trXMaxF":xmax
  "trYMinF":ymin
  "trYMaxF":ymax
  "vpXF":vpx0
  "vpYF":vpy1
  "vpWidthF":vpw
  "vpHeightF":vph
 end getvalues

 rx = r/(xmax-xmin)
 ry = rx*vpw/vph

 xpts1 = cos(th*pi/180)
 ypts1 = sin(th*pi/180)*scale

 xpts2 = xpts1*cos(rot*pi/180) - ypts1*sin(rot*pi/180)
 ypts2 = xpts1*sin(rot*pi/180) + ypts1*cos(rot*pi/180)

 xpts = rx*xpts2*(xmax-xmin) + x0
 ypts = ry*ypts2*(ymax-ymin) + y0
 str = unique_string("circle_ll")
 plt@$str$ = gsn_add_polyline(wks,plt,xpts,ypts,res1)
 delete(str)
 delete(xpts1)
 delete(ypts1)
 delete(xpts2)
 delete(ypts2)
 delete(th)
 delete(res1)

end
;---------------------------------------------
; 22 Oct 2014
; in respose to an ncl-talk question:
; Hopefully the following is helpful.  It returns both partial and 
; part correlations, but if you want you can ask for only one of them 
; to be returned using the optional argument "opt"
;
; e.g opt=True
;     opt@partialcorrelation=True
; Saji Hameed saji.nh@gmail.com
;-------------------

undef("mreg_part_corr")
function mreg_part_corr(x1,x2,y,opt)
local opt
begin
  x1x2 = escorc(x1,x2)
  tol = sqrt(1-x1x2^2)

  nx1 = dimsizes(x1)
  X = new( (/3,nx1/), float)
  X(0,:) = 1.0
  X(1,:) = x1
  X(2,:) = x2

  beta = reg_multlin(y, X, False)
  dreg = beta(1)
  nreg = beta(2)

  Xstd = dim_stddev(X)
  Ystd = stddev(y)
  XstdYstd = Xstd/Ystd
  B = beta*XstdYstd
  B(0) = 0.0
  part_corr_x1 = B(1)*tol
  part_corr_x2 = B(2)*tol
  partial_corr_x1 = part_corr_x1/sqrt((1-(escorc(y,x2))^2))
  partial_corr_x2 = part_corr_x2/sqrt((1-(escorc(y,x1))^2))
  if opt .and. isatt(opt,"part_correlation")
    if opt@part_correlation
    return((/part_corr_x1, part_corr_x2/))
    end if 
end if
  if opt .and. isatt(opt,"partal_correlation")
    if opt@partial_correlation
      return((/partial_corr_x1, partial_corr_x2/))
    end if
  end if
  res=(/partial_corr_x1,partial_corr_x2,part_corr_x1,part_corr_x2/)
  return(res)
end

;----------
; The following was posted as part of an ncl-talk question
; http://mailman.ucar.edu/pipermail/ncl-talk/2014-August/000231.html
; 
undef("eta2isop_wrf")
function eta2isop_wrf(fwrf, varName[1]:string, prs_iso[*]:numeric \
                     ,itime:integer, linlog[1]:integer, opt[1]:logical)
;
; interpolate a WRF variable on eta levels onto user specified isobaric levels
; Usage:
;        f = addfile(...)    , or,   f = addfiles(...)
;
;        plev = (/ 1000,925,850,700,600,500,400,300,250 \   ; integer, float, double
;                ,  200,150,100,70,50/)                               
;        plev!0     = "plev"
;        plev@units = "hPa"
;
;        linlog = 0
;        itime  = -1
;        z_iso = eta2isop_wrf(f, "geopotential", plev, itime, linlog, False)
;        printVarSummary(z_iso)

begin

  prs_eta  = wrf_user_getvar(fwrf, "pressure", itime)
  if (prs_eta@units.ne.p_iso@units) then
      print("eta2isop_wrf: pressure unit mismatch")
      print("eta2isop_wrf: prs_eta@units="+prs_eta@units)
      print("eta2isop_wrf: prs_iso@units="+prs_iso@units)
      exit
  end if

  var_eta  = wrf_user_getvar(fwrf,  varName  , itime)

  var_rank = dimsizes(dimsizes(var_eta))
  if (var_rank.eq.4) then         ;  (Time, bottom_top, south_north, west_east) => (0,1,2,3)
      var_iso = int2p_n_Wrap (prs_eta,var_eta,prs_iso,linlog,1)
  else if (var_rank.eq.3) then    ;  (bottom_top, south_north, west_east)       => (0,1,2)
           var_iso = int2p_n_Wrap (prs_eta,var_eta,prs_iso,linlog,0)
       else
           print("eta2isop_wrf: unexpected rank; rank="+var_rank+"; code expects rank=3 or 4 only")
           exit
       end if 
  end if

  var_iso@tag = "NCL: Simple interpolation to isobaric levels via int2p_n_Wrap"

  return(var_iso)
end
;--------------
undef("wgt_vertical_int")
function wgt_vertical_int (X:numeric, dp:numeric, lev_dim[1]:integer, iopt:integer)
; 
; Perform weighted vertical average (integral) or sum
; 
; Requirement: X must be in the following order: ([time,],lev,lat,lon)
;
; Nomenclature:
; X         - array to be integrated. No missing data allowed
; dp        - pressure thickness computed by "dpres_hybrid_ccm" or "dpres_plevel"
;             Must be the same size/shape as X
;             No missing data allowed.
; iopt      - =0 weighted vertical average
;             =1 weighted vertical sum
;             =2 weighted vertical sum, vertical avg
; lev_dim   - level dimension
;             lev_dim = 0                            ; (lev), (0); (time,lev),  (0,1) 
;                                                    ; (lev,lat,lon),  (0,1,2) 
;             lev_dim = 1                            ; (time,lev,lat,lon), (0,1,2,3)
;             lev_dim = 2                            ; (case,time,lev,lat,lon), (01,2,3,4)
;+++++++++++++++++++++++
; Usage for hybrid levels
;        f = addfile("....", "r")
;        hyai = f->hyai
;        hybi = f->hybi
;        p0   = f->P0                               ; p0=1000 or p0=100000
;        ps   = f->PS
;    
;        dp   = dpres_hybrid_ccm (ps,p0,hyai,hybi)  ; Pa [kg/(m s2)]
;--------------------
; Usage for pressure levels
;        f   = addfile("....", "r")
;        lev = f->lev  ; (/  1,  2,  3,  5,   7, 10, 20, 30, \   ; hPa 
;                           50, 70,100,150, 200,250,300,400, \
;                          500,600,700,775, 850,925,1000 /)
;                      ; uniys of lev and psfc must match
;        psfc= f->PS   ; PA         (time,lat,lon)
;        lev = lev*100 ; make PA to match psfc       
;        lev@units = "PA"
;
;        ptop= 0       ; integrate 0==>psfc at each grid point
;
;                      ; dp(klev,nlat,mlon) or dp(ntim,klev,nlat,mlon)
;        dp  = dpres_plevel(lev, psfc, ptop, 0)     ; Pa [kg/(m s2)]
;--------------------
; Use the 'dp' from above
;        t    = f->T                                ; (time,lev,lat,lon)
;        xvi  = wgt_vert_n(t, dp,  0, 1)

local dimX, dimDP, rankX, Xdp, vsum, wsum, vavg
begin

  dimX  = dimsizes( X )
  dimDP = dimsizes( dp )
  rankX = dimsizes( dimX )

  if (.not.all(dimDP.eq.dimX) ) then  ; error check
      print("wgt_vertical_int: dimension sizes are not equal")
      print("wgt_vertical_int:  dimX="+dimX)
      print("wgt_vertical_int: dimDP="+dimsizes(dp) )
      exit
  end if

  if (isatt(X,"_FillValue") .and. any(ismissing(X)) ) then  ; error check
      print("wgt_vertical_int: No _FillValue allowed")
      print("wgt_vertical_int: X: nFill="+num(ismissing(X)))
      exit
  end if

  if (isatt(dp,"_FillValue") .and. any(ismissing(dp)) ) then  ; error check
      print("wgt_vertical_int: No _FillValue allowed")
      print("wgt_vertical_int: dp: nFill="+num(ismissing(dp)))
      exit
  end if

  Xdp  = X*dp                                ; [? kg/(m s2)]   (temporary variable)
  copy_VarCoords(X, Xdp)

  vsum = dim_sum_n_Wrap( Xdp, lev_dim )      ; sum vertically [ie integrate]
  if (iopt.eq.1) then
      return(vsum)
  end if

  wsum = dim_sum_n_Wrap( dp , lev_dim )      ;  "      " 
  if (any(wsum.eq.0)) then
      if (.not.isatt(wsum,"_FillValue")) then
          wsum@_FillValue = default_fillvalue(typeof(wsum))
      end if
      wsum = where(wsum.eq.0, wsum@_FillValue, wsum)   ; avoid division by 0
  end if
      
  vavg = vsum/wsum                           ; one less dimension (no vertical dim)
  copy_VarMeta(vsum, vavg) 
  vavg@NCL_op = "Weighted Vertical Average"

  vsum@NCL_op = "Weighted Vertical Sum"
  wsum@NCL_op = "Summed Weights"

  if (iopt.eq.0) then
      return(vavg)
  end if

  return([/ vavg, vsum, wsum /] )    ; iopt=2
end
;--------------
undef ("get_nearest_grid_pts")
function get_nearest_grid_pts(obslat[*]:numeric, obslon[*]:numeric        \
                             ,ygrid:numeric, xgrid:numeric, opt[1]:logical)
; 
; Based upon great circle distance; find nearest grid point
; Input grid: ygrid and xgrid  could be [*] or [*][*]
; Can handle rectilinear, curvilinear, unstructured grids
;
local Nx, Ny, Nxg, Nyg, N, rankx, ranky, ylat2d, xlon2d, nm, dist, dmin, imin
begin

; error checks 

   Nx = dimsizes(obslon)           ; 'observation' locations (input)
   Ny = dimsizes(obslat)
   if (Nx.ne.Ny) then              ; observation locations must be same size
       print("get_nearest_grid_pts: obslon/obslat not same size: Nx="+Nx+"  Ny="+Ny)
       exit
   end if
 
   Nxg   = dimsizes(xgrid)
   Nyg   = dimsizes(ygrid)
   rankx = dimsizes(Nxg)           ; grid rank 
   ranky = dimsizes(Nxg)   
   
   if (rankx.ne.ranky .or. all(Nxg.ne.Nyg)) then       ; grid locations must be same size and shape
       print("get_nearest_grid_pts: xgrid/ygrid not same size and/or shape")
       print("xgrid="+Nxg)
       print("ygrid="+Nyg)
       exit
   end if

; allocate space; perform computations
; complexity due to handling rectilinear, curvilinear and unstructured grids
 
   N  = Nx                         ; number of observations
   dd = new( N, "double")          ; allocate space for distance     
   dd@long_name = "distance to grid point"
   dd@units     = "km"
 
   if (rankg.eq.1) then
       if (Nxg.ne.Nyg) then        ; rectilinear
           ylat2d = conform_dims( (/Nyg,Nxg/), ygrid, 0)  ; replicate to 2D; trick
           xlon2d = conform_dims( (/Nyg,Nxg/), xgrid, 1)
           ylat1d = ndtooned( ylat2d )
           xlat1d = ndtooned( xlon2d )

       else                                    ; unstructured (eg: spectral element) 
           nm = new( N, "integer")
           do n=0,N-1
              dist := gc_latlon(obslat(n),obslon(n), ygrid,xgrid, 2,4)  ; any units ok; choose km 
              dmin  = min( dist )              ; scalar
              nm(n) = ind( dist.eq.dmin(0) )   ; may be more than one; hence, dmin(0)
              dd(n) = (/ dmin /)               ; avoid a warning message since dd has meta data
           end do                              ; (/.../)  means transfer value only

           nm@long_name = "index to nearest grid pt"
           if (opt .and. isatt(opt,"distance")) then
               return( [/nm, dd/] )            ; return as list variable: index values and distances
            else
               return(nm)                      ; return index values as regular variable
           end if                              ; end opt
       end if
    else                                       ; curvilinear
       ylat1d = ndtooned( ygrid )
       xlat1d = ndtooned( xgrid )
   end if
                                               ; must be curvilinear or rectilinear
   nm = new( (/N,2/), "integer")
   do n=0,N-1
      dist := gc_latlon(obslat(n),obslon(n), lat1d,lon1d, 2,4)  ; any units ok; choose km
      dmin  = min( dist )
      imin  = ind(dist.eq.dmin(0))             ; may be more than one; hence, dmin(0)
      nm(n,:) = ind_resolve(imin, Nxg)
      dd(n) = (/ dmin /)                       ; avoid a warning message since dd has meta data
   end do                                      ; (/.../)  means transfer value only
 
   nm@long_name = "indices to nearest grid pt"
   if (opt .and. isatt(opt,"distance")) then
       return( [/nm, dd/] )                    ; return as list variable: index values and distance
   else
       return(nm)                              ; return index values as regular variable
   end if
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Description: Take an array of data, sort it, remove duplicates, and return 
;; Carl Schreck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
undef ( "remove_duplicates_nd" ) 
function remove_duplicates_nd( i_data:numeric ) 
local data, duplicate, uniqueInd, retVal       ; djs added
begin 
  data = ndtooned( i_data ) 
  qsort(data) 
  duplicate = new( dimsizes(data), logical ) 
  duplicate(0) = False 
  do i = 1, dimsizes(data)-1 
    duplicate(i) = ( data(i).eq.data(i-1) ) 
  end do 
  uniqueInd = ind(.not.duplicate) 
  retVal = data(uniqueInd) 
  return(retVal) 
end ; remove_duplicates 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Description: Take an array of data, sort it, remove duplicates, and return 
;; slight variation of Carl Schreck's "remove_duplicates"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
undef ( "remove_duplicates_1d" ) 
function remove_duplicates_1d( i_data[*]:numeric ) 
local data, dim_data, duplicate, uniqueInd, retVal  
begin 
  data     = dim_pqsort(i_data,2)
  dim_data = dimsizes(data) 

  duplicate = new( dim_data, logical ) 
  duplicate(0) = False 
  do i = 1, dim_data-1 
    duplicate(i) = ( data(i).eq.data(i-1) ) 
  end do 

  uniqueInd = ind(.not.duplicate) 
  retVal    = data(uniqueInd) 
  return(retVal) 
end ; remove_duplicates 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Add a marker to a histogram plot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
undef("histogram_interp")
function histogram_interp(MidBarLocs[*]:numeric, BinLocs[*]:numeric, xHstMarkVals[*]:numeric)
local nMid, nBinLocs, MidBarVals, xHstBinLocs
begin
  nMid        = dimsizes(MidBarLocs)
  nBinLocs    = dimsizes(BinLocs)
  if (nBinLocs.ne.(nMid+1)) then
      print("histogram_interp: size mismatch:  nMid="+nMid+" nBinLocs="+nBinLocs)
      exit
  end if
  MidBarVals  = 0.5*(BinLocs(0:nBinLocs-2)+BinLocs(1:nBinLocs-1))

              ; simple linear interpoaltion
  xHstLocs    = linint1(MidBarVals,MidBarLocs, False,xHstMarkVals , 0)
  xHstLocs@long_name = "NDC coordinates of marker location(s)"

  return (xHstLocs)               
end
;---
               ; USER LEVEL FUNCTION; this could also be histogram_LINE or histogram_TICKMARK
undef("histogram_mark")      
procedure histogram_mark(wks[1]:graphic, hst[1]:graphic, xHstMarkLocs[*]:numeric, yHstMarkLocs[*]:numeric, pmHst[1]:logical)
local xHstLocs, pnred, nMarkX, nMarkY
begin
  nMarkX = dimsizes(xHstMarkLocs)
  nMarkY = dimsizes(yHstMarkLocs)
  if (nMarkX.ne.nMarkY) then
      print("histogram_mark: nMarkX must equal nMarkY:  nMarkX="+nMarkX+" nMarkY="+nMarkY)
      exit
  end if
           ; interpolate along the 'x' NDC coords
  xHstLocs = histogram_interp(hst@MidBarLocs, hst@BinLocs, xHstMarkLocs)

; local default resource settings

  pmres                = True         ;-- local
  pmres@gsClipOn       = False        ;-- allow to extend beyond the view port
  pmres@gsMarkerColor  = "magenta"    ;-- default marker color
  pmres@gsMarkerSizeF  =  0.0125      ;-- default; set size of marker
  pmres@gsMarkerThicknessF = 3.0      ;-- default is 1.0
 ;pmres@gsMarkerIndex  =   8          ;-- down triangle
  pmres@gsMarkerIndex  =   0          ;-- default; asterisk

; allow users to over-ride the local bootstrap
            
  if (pmHst .and. isatt(pmHst,"gsMarkerColor")) then
      pmres@gsMarkerColor  = pmHst@gsMarkerColor
  end if

  if (pmHst .and. isatt(pmHst,"gsMarkerSizeF")) then
      pmres@gsMarkerSizeF  = pmHst@gsMarkerSizeF
  end if

  if (pmHst .and. isatt(pmHst,"gsMarkerIndex")) then
      pmres@gsMarkerIndex  = pmHst@gsMarkerIndex
  end if

  if (pmHst .and. isatt(pmHst,"gsMarkerThicknessF")) then
      pmres@gsMarkerThicknessF  = pmHst@gsMarkerThicknessF
  end if

  if (pmHst .and. isatt(pmHst,"gsClipOn")) then   ; NCL default is True
      pmres@gsClipOn = pmHst@gsClipOn
  end if

;-- Add the marker(s)
  
  do n=0,nMarkX-1
     hst@$unique_string("hstMark_")$ = gsn_add_polymarker(wks, hst, xHstLocs(n), yHstMarkLocs(n) , pmres)  
  end do

;-- No return since this is a procedure

end
;--
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Description: Create a legend using calls to gsn_add_polyline/gsn_add_text. 
;; Resources are used to set the position of upper-left corner of the legend, 
;; the legend labels, the spacing of the legend items, and the appearance of 
;; the lines and labels.  
;; Adam Phillips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
undef("simple_legend") 
function simple_legend(wksp:graphic,xyplot:graphic,genres:logical,lineres:logical,labelres:logical)
local wksp,xyplot,genres,lineres,labelres,XPos,YPos,numitems,xrange,yrange,incr,y_pos,x_line_start,\
      x_line_end,x_text_start,x_text_end

begin
  getvalues xyplot
    "trYMinF" : ymin
    "trYMaxF" : ymax
    "trXMinF" : xmin
    "trXMaxF" : xmax
  end getvalues
  yrange = ymax - ymin
  xrange = xmax - xmin

  if (.not.isatt(genres,"ItemSpacePercent")) then
     genres@ItemSpacePercent = 5.
  end if
  incr = (genres@ItemSpacePercent/100.)*yrange*-1.

  if (.not.isatt(genres,"Position")) then   ; set default
     genres@Position = "Top"
  end if

  if (isatt(labelres,"lgLabels")) then
     numItems = dimsizes(labelres@lgLabels)
  else
     print("simple_legend: Fatal: Legend labels must be set using lgLabels. If empty labels are required set lgLabels resource to an empty string array dimenensioned equal to the number of legend lines.")
     exit
  end if

  if (isatt(genres,"XPosPercent")) then
     XPos = genres@XPosPercent/100.
  else
     XPos = 0.05     ; set default
  end if

  if (isatt(genres,"YPosPercent")) then
     YPos = genres@YPosPercent/100.
     if (genres@Position.eq."Bottom") then  ; modify YPosPercent when doing bottom label so that first label is on top
        YPos = YPos+((numItems-1)*(genres@ItemSpacePercent/100.))
     end if
  else
     if (genres@Position.eq."Bottom") then
        YPos = 0.05   ; set default
     else
        YPos = 0.95   ; set default
     end if
  end if

  txres = True
  txres@txJust = "CenterLeft"
  txres@txFontHeightF = 0.015

  zres = True

  polyres = True
  do gg = 0,numItems-1
     if (gg.eq.0) then
        y_pos = (YPos*yrange)+ymin
     else
        y_pos = y_pos+incr
     end if
     x_line_start = xmin+(XPos*xrange)   ; position of start of legend line
     if (isatt(lineres,"LineLengthPercent")) then
        x_line_end = x_line_start+(xrange*(lineres@LineLengthPercent/100.))
     else
        x_line_end = x_line_start+(xrange*.07)    ; set default
     end if
     if (isatt(genres,"LineLabelWhiteSpacePercent")) then
        x_text_start = x_line_end+(xrange*(genres@LineLabelWhiteSpacePercent/100.))
     else
        x_text_start = x_line_end+(xrange*0.03)    ; set default
     end if
     if (isatt(lineres,"lgLineColors")) then
        if (dimsizes(lineres@lgLineColors).eq.1.or.dimsizes(lineres@lgLineColors).ne.dimsizes(labelres@lgLabels)) then
           zres@gsLineColor = lineres@lgLineColors(0)
        else
           zres@gsLineColor = lineres@lgLineColors(gg)
        end if
     end if
     if (isatt(lineres,"lgLineOpacity")) then
        if (dimsizes(lineres@lgLineOpacity).eq.1.or.dimsizes(lineres@lgLineOpacity).ne.dimsizes(labelres@lgLabels)) then
           zres@gsLineOpacityF = lineres@lgLineOpacity(0)
        else
           zres@gsLineOpacityF = lineres@lgLineOpacity(gg) 
        end if
     end if    
     if (isatt(lineres,"lgLineThicknesses")) then 
        if (dimsizes(lineres@lgLineThicknesses).eq.1.or.dimsizes(lineres@lgLineThicknesses).ne.dimsizes(labelres@lgLabels)) then
           zres@gsLineThicknessF = lineres@lgLineThicknesses(0)
        else
           zres@gsLineThicknessF = lineres@lgLineThicknesses(gg)
        end if
     end if
     if (isatt(lineres,"lgDashIndexes")) then 
        if (dimsizes(lineres@lgDashIndexes).eq.1.or.dimsizes(lineres@lgDashIndexes).ne.dimsizes(labelres@lgLabels)) then
           zres@gsLineDashPattern = lineres@lgDashIndexes(0)
        else
           zres@gsLineDashPattern = lineres@lgDashIndexes(gg)
        end if
     else
        zres@gsLineDashPattern = 0
     end if 
     xyplot@$unique_string("lg")$ = gsn_add_polyline(wksp,xyplot,(/x_line_start,x_line_end/),(/y_pos,y_pos/),zres)

     if (isatt(labelres,"lgLabelFontHeights")) then
        if (dimsizes(labelres@lgLabelFontHeights).eq.1.or.dimsizes(labelres@lgLabelFontHeights).ne.dimsizes(labelres@lgLabels)) then
           txres@txFontHeightF = labelres@lgLabelFontHeights(0)
        else
           txres@txFontHeightF = labelres@lgLabelFontHeights(gg)
        end if
     end if
     if (isatt(labelres,"lgLabelColors")) then
        if (dimsizes(labelres@lgLabelColors).eq.1.or.dimsizes(labelres@lgLabelColors).ne.dimsizes(labelres@lgLabels)) then
           txres@txFontColor = labelres@lgLabelColors(0)
        else
           txres@txFontColor = labelres@lgLabelColors(gg)
        end if
     end if
     if (isatt(labelres,"lgLabelOpacity")) then
        if (dimsizes(labelres@lgLabelOpacity).eq.1.or.dimsizes(labelres@lgLabelOpacity).ne.dimsizes(labelres@lgLabels)) then
           txres@txFontOpacityF = labelres@lgLabelOpacity(0)
        else
            txres@txFontOpacityF = labelres@lgLabelOpacity(gg)
        end if
     end if
     xyplot@$unique_string("lg")$ = gsn_add_text(wksp,xyplot,labelres@lgLabels(gg),x_text_start,y_pos,txres)
  end do
  return(xyplot)
end

