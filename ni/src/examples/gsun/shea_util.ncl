;
;      $Id: shea_util.ncl,v 1.18 2004-12-15 20:10:21 shea Exp $
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
; ********************************************************
; Function/Procedures included here can call built-in functions
; and functions contained within the gsn*.ncl and contributed.ncl
; libraries. 
; ------------------------------------------------------------
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
function ZeroNegDashLineContour (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line twice as thick and set neg contourlines to dash
; Dash line patterns: http://ngwww.ucar.edu/ngdoc/ng/ref/dashpatterns.html

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
function ZeroNegDashLineContourOverlay (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line twice as thick and set neg contourlines to dash
; Dash line patterns: http://ngwww.ucar.edu/ngdoc/ng/ref/dashpatterns.html

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
function ZeroGoneNegDashLineContour(plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Minimizes the zero line and set neg contourlines to dash
; Dash line patterns: http://ngwww.ucar.edu/ngdoc/ng/ref/dashpatterns.html
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
function NegDashLineContour (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Set neg contourlines to dash
; Dash line patterns: http://ngwww.ucar.edu/ngdoc/ng/ref/dashpatterns.html

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
function ShadeLtContour (plot:graphic, CnLt:float, FillPatLt:integer ) 

; operates on a plot object created by "gsn_csm.ncl" code
; Fill all contours less than "CnLt" to the
;      pattern corresponding to "FillPatLt" 
;      http://ngwww.ucar.edu/ngdoc/ng/ref/fillpatterns.html

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
         if(cnlvls(i).le.CnLt) then      ; different criteria.
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
function ShadeGtContour (plot:graphic, CnGt:float, FillPatGt:integer ) 

; operates on a plot object created by "gsn_csm.ncl" code
; Fill all contours greater than "CnGt" to the
;      pattern corresponding to "FillPatGt" 
;      http://ngwww.ucar.edu/ngdoc/ng/ref/fillpatterns.html

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
function ShadeLtGtContour (plot:graphic
                          ,CnLt:float, FillPatLt:integer\
                          ,CnGt:float, FillPatGt:integer ) 

; operates on a contour object created by "gsn_csm.ncl" code
; Fill all contours less-than/greater-than "CnLt/CnGt" to the
;      pattern corresponding to "FillPat" 
;      http://ngwww.ucar.edu/ngdoc/ng/ref/fillpatterns.html

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
function ShadeGeLeContour (plot:graphic
                          ,CnGe:float, CnLe:float, FillPat:integer ) 

; operates on a contour object created by "gsn_csm.ncl" code
; Fill all contours greater-than-eq2/less-than-eq2 "CnGe/CnLe" to the
;      pattern corresponding to "FillPat" 
;      http://ngwww.ucar.edu/ngdoc/ng/ref/fillpatterns.html

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
function ShadeGeLeContourDensity (plot:graphic ,CnGe:float, CnLe:float \
                                 ,FillPat:integer, FillDensity:float ) 

; operates on a contour object created by "gsn_csm.ncl" code
; Fill all contours greater-than-eq2/less-than-eq2 "CnGe/CnLe" to the
;      pattern corresponding to "FillPat" 
;      http://ngwww.ucar.edu/ngdoc/ng/ref/fillpatterns.html

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
procedure genCmapColorWheelInternal (wks:graphic, ncol:integer, hueRange:float)
local hleft, hright, colorWheel, h, s, v, cmap
begin
   hleft      = fabs(hueRange(0))
   hright     = fabs(hueRange(1))
   colorWheel = hueRange(2)

   h          = new (ncol,float) 
   s          = new (ncol,float) 
   v          = new (ncol,float)        ; keep at 1.0 

   sMax       = 0.90                    ; 1.0/0.8 fine also 
   sMin       = 0.15                       
   if (colorWheel.eq.0.) then
       nc2           = ncol/2
       h(0:nc2-1)    = hleft
       h(nc2:ncol-1) = hright

       s(0:nc2-1)    = fspan(sMax,sMin,nc2)
       s(nc2:ncol-1) = fspan(sMin,sMax,nc2)

       v(0:ncol-1)   = 1.0
      ;vmax          = 1.00
      ;vmin          = 1.00
      ;v(0:nc2-1)    = fspan(vmax,vmin,nc2)
      ;v(nc2:ncol-1) = fspan(vmin,vmax,nc2)
   else 
       print ("genCmapColorWheelInternal: hleft="+hleft+" hright="+hright)
       h             = new (ncol,float) 
       h(0:ncol-1)   = fspan(hleft,hright,ncol)
       s(0:ncol-1)   = sMax                      ; keep constant
       v(0:ncol-1)   = 1.0
       print ("genCmapColorWheelInternal: h="+h+"  s="+s)
   end if 

   cmap             = new((/ncol+3,3/),float); create array
   cmap(0,:)        = (/1.,1.,1./)           ; white background
   cmap(1,:)        = (/0.,0.,0./)           ; black foreground
   cmap(2:ncol+1,:) = hsv2rgb(h,s,v)
   cmap(  ncol+2,:) = (/0.90, 0.90, 0.90 /)  ; append gray

  ;print ("ncol="+ncol)
  ;print ("h="+h +"  s="+s+"  v="+v)
  ;do n=0,ncol+1
  ;   print ("cmap="+n+"  "+cmap(n,0)+"  "+cmap(n,1)+"  "+cmap(n,2))
  ;end do

   gsn_define_colormap(wks, cmap)
end
;-------------------------------------------------------------------
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
procedure genCmapNameCol (wks:graphic, colors[*]:string)
; This works but an idiosyncracy of the underlying graphics is
; that it starts at cmap(2,:) and cycles thru the colors.
; However, on all succeeding passes it starts at (cmap1,:).

; generates RGB values for input named colors 
; http://www.scd.ucar.edu/zine/99/spring/text/gfx/2.gifs.html
; http://www.scd.ucar.edu/zine/99/spring/text/gfx/rgb.txt
; http://ngwww.ucar.edu/ngdoc/ng4.1.1beta/ref/colorspecs.html#NhlTColorDefinitionGenArray

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

; txJust: http://ngwww.ucar.edu/ngdoc/ng4.1/./qsg/textitem/tx06.html

local yBot, xLeft, xRight, txres, TimeStamp
begin
   yBot                = 0.001
   xLeft               = 0.001
   xRight              = 0.999
                                        ; Sylvia Murphy mods
   if (NhlClassName(wks).eq."psWorkstationClass") then
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
; http://ngwww.ucar.edu/ngdoc/ng/ref/fillpatterns.html
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
        colors = gsn_retrieve_colormap(wks)	;Add gray color to color map
        nu = dimsizes(colors)
        numb = nu(0)
        cmap = new ((/numb+1,3/),float)
        cmap(:numb-1,:) = colors
        cmap(numb,:) = (/.72,.72,.72/)
        gsn_define_colormap(wks,cmap)
	delete(numb)
	delete(nu)
	delete(cmap)
	delete(colors)

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
function ShadeCOI(wks:graphic,plot:graphic,w,time, gsres:logical)
local newtime, newcoi, ni, indices, do_both
begin
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
  set_attr(gsres,"gsFillIndex",5)
  set_attr(gsres,"gsEdgeThicknessF",3.)
  set_attr(gsres,"gsEdgesOn",True)

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
    newtime = new(ni+7,typeof(time))
    newcoi  = new(ni+7,typeof(w@coi))

    newtime(0)      = xmin
    newtime(1)      = time(indices(0))
    newtime(2:ni+1) = time(indices)
    newtime(ni+2)   = time(indices(dimsizes(indices)-1))
    newtime(ni+3)   = xmax
    newtime(ni+4)   = xmax
    newtime(ni+5)   = xmin
    newtime(ni+6)   = xmin

    newcoi(0)      = ymin
    newcoi(1)      = ymin
    newcoi(2:ni+1) = w@coi(indices)
    newcoi(ni+2)   = ymin
    newcoi(ni+3)   = ymin
    newcoi(ni+4)   = ymax
    newcoi(ni+5)   = ymax
    newcoi(ni+6)   = ymin
  else
;
; Create some arrays to hold the polygon.
;
    newtime = new(ni+6,typeof(time))
    newcoi  = new(ni+6,typeof(w@coi))
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
      newtime(1)      = time(indices(0))
      newtime(2:ni+1) = time(indices)
      newtime(ni+2)   = xmax
      newtime(ni+3)   = xmax
      newtime(ni+4)   = xmin
      newtime(ni+5)   = xmin
  
      newcoi(0)       = ymin
      newcoi(1)       = ymin
      newcoi(2:ni+1)  = w@coi(indices)
      newcoi(ni+2)    = w@coi(indices(ni-1))
      newcoi(ni+3)    = ymax
      newcoi(ni+4)    = ymax
      newcoi(ni+5)    = ymin
    else
      newtime(0)      = xmin
      newtime(1)      = xmin
      newtime(2:ni+1) = time(indices)
      newtime(ni+2)   = time(indices(ni-1))
      newtime(ni+3)   = xmax
      newtime(ni+4)   = xmax
      newtime(ni+5)   = xmin
  
      newcoi(0)       = ymax
      newcoi(1)       = w@coi(indices(0))
      newcoi(2:ni+1)  = w@coi(indices)
      newcoi(ni+2)    = ymin
      newcoi(ni+3)    = ymin
      newcoi(ni+4)    = ymax
      newcoi(ni+5)    = ymax
    end if
  end if
;
; Add cone of influence polygon.
;
  coi           = gsn_add_polygon(wks,plot,newtime,newcoi,gsres)
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
;*************************************************************************
; Adam Philips

function get_script_name()
local args, nargs, found, i, cstr, ncstr, script_name
begin
;  Entering retrieve program name section (compliments of Mary)
   	args  = systemfunc("ps -o args") ;Find the string that starts with "ncl"
   	nargs = dimsizes(args)	    ; This is the 1 that has the script name.
   	found = False     ; logical indicatating whether we found "ncl " string.
	i     = 0            ; "args" counter
        do while (i.lt.nargs.and..not.found)
           cstr = stringtochar(args(i)) ;Convert to character array so we can 		
					;parse it.
           ncstr = dimsizes(cstr)  ;Assume that the script is called "xxx.ncl"
;           print(cstr)
           if (ncstr.gt.4.and.chartostring(cstr(0:3)).eq."ncl ") then	
              if (ncstr.gt.9) then     ;grabs "ncl file.ncl" 
                 script_name = chartostring(cstr(4:ncstr-6))
              else
                 script_name = chartostring(cstr(4:))
              end if
              found = True           ; Found the "ncl " string.
           end if
           if (ncstr.gt.4.and.chartostring(cstr(0:3)).eq."ncl.") then	
	      if (ncstr.gt.9) then	;grabs "ncl.*** file.ncl"
                 script_name = chartostring(cstr(8:ncstr-6))
              else
	         script_name = chartostring(cstr(4:))
	      end if
 	      found = True           ; Found the "ncl.*" string.
	   end if  
           i = i + 1     ; increment "args" counter
           delete(cstr)	; Delete cstr, changes sizes each time through
   	end do

   	if (found) then
           return(script_name)
    	else
           print("No script name found, make sure script was run without the '<' character.")
     	   return("missing")
   	end if 
end
; ----------------------------------------------------------
; Adam Phillips
;
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
; txJust: http://ngwww.ucar.edu/ngdoc/ng4.1/./qsg/textitem/tx06.html

local yBot, xLeft, xRight, xoff_set, yoff_set,txres, TimeStamp, script_name \
    , ps_xLeft, ps_yLeft, ps_xRight, ps_orient
begin
   yBot                = 0.001
   xLeft               = 0.001
   xRight              = 0.999
                                        ; Sylvia Murphy mods
   if (NhlClassName(wks).eq."psWorkstationClass") then
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
   script_name = systemfunc("pwd")+"/"+get_script_name()+".ncl"

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
	   if (inlon.ge.361) then
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
function boxplot (wks:graphic,x[*]:numeric,y[*][*]:numeric,boxOpts:logical,plotres:logical,lineres:logical)
;
;	This function creates a blank linLog plot object, on which box plots are created by extensive
;	use of gsn_add_polyline. The user can draw as many or as few box plots as they wish.
;	The plot is not drawn, and the frame is not advanced. May be used with gsn_panel.
;	The function options are as follows:
;
;	wks     ----- The workstation
;	x[*]    ----- A one-dimensional array containing the X-axis values of where the box plot(s)
;	              will be drawn.
;	y[*][*] ----- A two-dimensional array, where the rightmost dimension contains the box plot
;	 	      reference pt. data. y(n,0)=bottom_value, y(n,1)=bottom_value_of_box,
;		      y(n,2)=mid-value_of_box,y(n,3)=top_value_of_box,y(n,4)=top_value
;	boxOpts ----- 2 options attached as attributes may be attached here. 
;		      boxWidth ---- Sets the width of the box.
;		      boxColors ---- An array that contains the colors that the boxes will be drawn in.
;			             Ex. boxOpts@boxColors =  (/"green","blue"/)
;				     If the number of colors specified does not equal the number of 
;				     boxes drawn, only the first color specified will be used.
;	plotres ----- An optional xy-plot resource list. Will override defaults where applicable.
;	lineres ----- An optional resource list pertaining to the lines used to draw the boxes/lines.
;
;	Example call: plot3 = boxplot(wks,ntime,newyval,opti,res,False)
;		      draw(plot3)
;		      frame(wks)
;
begin
	dimquery = dimsizes(y)
	numbox = dimquery(0)

	if (numbox.ne.dimsizes(x)) then
	   print("Fatal: X must be one-dimensional and both X and Y must have the same rightmost dimension")
	   exit
	end if

;	Developing x-axis
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
	      boxwidth = boxOpts@boxWidth
	   else
	      boxwidth = dx*.3
	   end if
	else
	   boxwidth = dx*.3
	end if	
	
		
	labarr = new(numbox+2,"string")		;Prepare actual X-axis labels...
	labarr(0) = ""
	labarr(numbox+1) = ""
	labarr(1:numbox) = xAxis(1:numbox)	

	if (plotres) then
	   print("Plot resources detected, accepting")
	   fsatts = getvaratts(plotres)
	   do ty = 0,dimsizes(fsatts)-1
	      if (fsatts(ty).eq."tmXBLabels") then		;Special section to test for XBLabels
	         if (dimsizes(plotres@tmXBLabels).ne.numbox) then
		    print("Fatal:Number of XB Labels does not match number of boxes, exiting")
		    exit
		 else
		    labarr(1:numbox) = plotres@$fsatts(ty)$
		 end if
		 xblab = plotres@tmXBLabels
		 delete(plotres@tmXBLabels)		;Delete so tmXBLabels is not used when all
	      end if					;atts are assigned below...
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
	end create
	if (plotres) then
	   attsetvalues(plot,plotres)	
	end if
		
	polyres = True						;Set up defaults
 	polyres@gsLineColor       = "black"                     ; color of lines
        polyres@gsLineThicknessF  = 1.5                       	; thickness of lines
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
	            print("warning:Number of colors provided does not match number of boxes, using only one color")
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
	   xx = (/(ff-(boxwidth/8.)),(ff+(boxwidth/8.))/)
	   dum(gg,0) = gsn_add_polyline(wks,plot,xx,yy,polyres)   

	   yy = (/y(gg,3),y(gg,4)/)
	   xx = (/ff,ff/)
	   polyres@gsLineDashPattern = 1
	   dum(gg,1) = gsn_add_polyline(wks,plot,xx,yy,polyres)   
	   polyres@gsLineDashPattern = 0

	   yy = (/y(gg,3),y(gg,3)/)
	   xx = (/(ff-(boxwidth/2.)),(ff+(boxwidth/2.))/)
	   dum(gg,2) = gsn_add_polyline(wks,plot,xx,yy,polyres)

	   yy = (/y(gg,1),y(gg,3)/)
	   xx = (/(ff-(boxwidth/2.)),(ff-(boxwidth/2.))/)
	   dum(gg,3) = gsn_add_polyline(wks,plot,xx,yy,polyres)   

	   yy = (/y(gg,2),y(gg,2)/)
	   xx = (/(ff-(boxwidth/2.)),(ff+(boxwidth/2.))/)
	   dum(gg,4) = gsn_add_polyline(wks,plot,xx,yy,polyres)   

	   yy = (/y(gg,1),y(gg,3)/)
	   xx = (/(ff+(boxwidth/2.)),(ff+(boxwidth/2.))/)
	   dum(gg,5) = gsn_add_polyline(wks,plot,xx,yy,polyres)   

	   yy = (/y(gg,1),y(gg,1)/)
	   xx = (/(ff-(boxwidth/2.)),(ff+(boxwidth/2.))/)
	   dum(gg,6) = gsn_add_polyline(wks,plot,xx,yy,polyres)   

	   yy = (/y(gg,0),y(gg,1)/)
	   xx = (/ff,ff/)
	   polyres@gsLineDashPattern = 1
	   dum(gg,7) = gsn_add_polyline(wks,plot,xx,yy,polyres)   
	   polyres@gsLineDashPattern = 0

	   yy = (/y(gg,0),y(gg,0)/)
	   xx = (/(ff-(boxwidth/8.)),(ff+(boxwidth/8.))/)
	   dum(gg,8) = gsn_add_polyline(wks,plot,xx,yy,polyres)
	end do
	dumname = unique_string("dum")								
	plot@$dumname$ = dum									
	if (isvar("xblab").eq."True") then							
	   plotres@tmXBLabels = xblab		;reassign XBLabels for possible future plots	
	end if				
	return(plot)
end

	         
	         
