; ******************** NOTE *******************************
; These are my personal functions/procedures. They do things
; the way I want them. If u want to modify one to suit
; your tastes then I suggest u copy this file or an 
; individual routine, rename it and then modify it as u wish.
; 
; I will try to help people out but these routines are NOT
; meant to be general purpose.
; ********************************************************
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
local eps, indEps
begin
       cnlvls = CNLVLS                          ; historical
       if (dimsizes(cnlvls).gt.1) then
          ;eps    = 1.e-09                          ; arbitrary
           eps    = 1.e-05                          ; arbitrary
           indEps=ind(fabs(cnlvls).le.eps) 
           if (.not.ismissing(indEps)) then
               cnlvls(indEps) = 0.0        ; the "zero" line ==>-0.8e-09
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
;-------------------------------------------------------------------
function get_cnLevels (plot:graphic) 
local cnlvls
begin
       if (isatt(plot,"contour")) then
           getvalues plot@contour       
             "cnLevels"   : cnlvls
           end getvalues
       else
           getvalues plot
             "cnLevels"   : cnlvls
           end getvalues
       end if
       return(cnlvls)
end
; -------------------------------------------------------
function get_flags (plot:graphic) 
begin
       if (isatt(plot,"contour")) then
          getvalues plot@contour       
             "cnLevelFlags"   : flags
          end getvalues
       else
          getvalues plot       
             "cnLevelFlags"   : flags
          end getvalues 
       end if
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

       if (isatt(plot,"contour")) then
           setvalues plot@contour
             "cnMonoLineThickness"    : False
             "cnLineThicknesses"      : cnlinethk
           end setvalues
       else
           setvalues plot
             "cnMonoLineThickness"    : False
             "cnLineThicknesses"      : cnlinethk
           end setvalues
       end if

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

       if (isatt(plot,"contour")) then
           setvalues plot@contour
             "cnMonoLineThickness"    : False
             "cnLineThicknesses"      : cnlinethk
           end setvalues
       else
           setvalues plot
             "cnMonoLineThickness"    : False
             "cnLineThicknesses"      : cnlinethk
           end setvalues
       end if

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
    
           if (isatt(plot,"contour")) then
               setvalues plot@contour
                 "cnMonoLineDashPattern"  : False
                 "cnLineDashPatterns"     : cnlinepat
                 "cnMonoLineThickness"    : False
                 "cnLineThicknesses"      : cnlinethk
               end setvalues
           else
               setvalues plot
                 "cnMonoLineDashPattern"  : False
                 "cnLineDashPatterns"     : cnlinepat
                 "cnMonoLineThickness"    : False
                 "cnLineThicknesses"      : cnlinethk
               end setvalues
           end if
      end if                                ; any

      return (plot)

end
; -------------------------------------------------------
function ZeroNegDashLineContourOverlay (plot:graphic) 

; operates on a plot object created by "gsn_csm.ncl" code
; Make zero line twice as thick and set neg contourlines to dash
; Dash line patterns: http://ngwww.ucar.edu/ngdoc/ng/ref/dashpatterns.html

local cnlvls, cnlinepat, cnlinethk, n, N
begin
       if (isatt(plot,"contour2")) then
           getvalues plot@contour2       
             "cnLevels"   : cnlvls
           end getvalues
       else
           getvalues plot
             "cnLevels"   : cnlvls
           end getvalues
       end if

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
    
           if (isatt(plot,"contour2")) then
               setvalues plot@contour2
                 "cnMonoLineDashPattern"  : False
                 "cnLineDashPatterns"     : cnlinepat
                 "cnMonoLineThickness"    : False
                 "cnLineThicknesses"      : cnlinethk
               end setvalues
           else
               setvalues plot
                 "cnMonoLineDashPattern"  : False
                 "cnLineDashPatterns"     : cnlinepat
                 "cnMonoLineThickness"    : False
                 "cnLineThicknesses"      : cnlinethk
               end setvalues
           end if
      end if                                ; any

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

    if (isatt(plot,"contour")) then
        setvalues plot@contour
           "cnMonoLineDashPattern"  : False
           "cnLineDashPatterns"     : cnlinepat
           "cnMonoLevelFlag"        : False     ; this is the default
           "cnLevelFlags"           : cnlevflg 
        end setvalues
    else
        setvalues plot
           "cnMonoLineDashPattern"  : False
           "cnLineDashPatterns"     : cnlinepat
           "cnMonoLevelFlag"        : False    ; but here for safety
           "cnLevelFlags"           : cnlevflg 
        end setvalues
    end if
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
    
           if (isatt(plot,"contour")) then
               setvalues plot@contour       
                 "cnMonoLineDashPattern"  : False
                 "cnLineDashPatterns"     : cnlinepat
               end setvalues
           else
               setvalues plot
                 "cnMonoLineDashPattern"  : False
                 "cnLineDashPatterns"     : cnlinepat
               end setvalues
           end if
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
     
       if (isatt(plot,"contour")) then
           setvalues plot@contour           ; Retrieve contour levels.
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       else
           setvalues plot
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       end if
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
     
       if (isatt(plot,"contour")) then
           setvalues plot@contour           ; Retrieve contour levels.
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       else
           setvalues plot
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       end if
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
     
       if (isatt(plot,"contour")) then
           setvalues plot@contour           ; Retrieve contour levels.
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       else
           setvalues plot
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       end if
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
     
       if (isatt(plot,"contour")) then
           setvalues plot@contour               ; Retrieve contour levels.
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       else
           setvalues plot
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
           end setvalues
       end if
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
     
       if (isatt(plot,"contour")) then
           setvalues plot@contour               ; Retrieve contour levels.
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
             "cnFillScaleF"      : FillDensity  ; 'density' of lines/periods
           end setvalues
       else
           setvalues plot
             "cnFillOn"          : True
             "cnMonoFillColor"   : True
             "cnMonoFillPattern" : False
             "cnFillPatterns"    : patterns
             "cnFillScaleF"      : FillDensity  ; 'density' of lines/periods
           end setvalues
       end if
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
        
 if (isatt(plot,"contour")) then
     getvalues plot@contour
       "cnLevels"     : cnlvls
       "cnFillColors" : colors
     end getvalues
 else
     getvalues plot
        "cnLevels"     : cnlvls
        "cnFillColors" : colors
     end getvalues
 end if

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
; ------------------------------------------------------------------
function plot_xy2 (wks:graphic, x:numeric, yL:numeric,yR:numeric
                              , resL:logical, resR:logical)
 begin
    locResL = True
    if (resL) then
        locResL = resL           ; copy input to local resources
    end if
    AttsL  = getvaratts(locResL) ; input attributes
                                 ; The following section is not
                                 ; needed. it justs prevents
    if (.not.any(ismissing(AttsL))) then   ; annoying warning msgs
        if (any(AttsL.eq."trXMinF")) then  ; from being printed 
            locResL@xyComputeXMin = False 
        end if                             
        if (any(AttsL.eq."trXMaxF")) then
            locResL@xyComputeXMax = False  ; same
        end if
        if (any(AttsL.eq."trYMinF")) then
            locResL@xyComputeYMin = False  ; same
        end if                            
        if (any(AttsL.eq."trYMaxF")) then
            locResL@xyComputeYMax = False  ; same
        end if
    end if

    locResL@gsnFrame   = False  ; Don't auto advance the
    locResL@gsnDraw    = False  ; frame or draw the plot when
                                ; gsn_xy is called.
    locResL@tmYROn     = False  ; Turn off right tick marks.
    locResL@tmYRMinorOn= False  ; Turn off right minor tick marks.
    locResL@tmYUseLeft = False  ; Keep independent of right

    xyL = gsn_xy (wks,x,yL,locResL)

    if (.not.any(ismissing(AttsL))) then   
        if (any(AttsL.eq."tmYLMode")) then
            setvalues xyL
              "tmYLMode"   : "Explicit"
              "tmYLValues" : locResL@tmYLValues 
              "tmYLLabels" : locResL@tmYLLabels 
            end setvalues
        end if
    end if

   ;draw (xyL)

    locResR = True
    if (resR) then
        locResR = resR           ; copy input to local resources
    end if
    AttsR  = getvaratts(locResR) ; input attributes
                                 ; The following section is not
                                 ; needed. it justs prevents
    if (.not.any(ismissing(AttsR))) then   ; annoying warning msgs
        if (any(AttsR.eq."trXMinF")) then  ; from being printed 
            locResR@xyComputeXMin = False 
        end if                             
        if (any(AttsR.eq."trXMaxF")) then
            locResR@xyComputeXMax = False  ; same
        end if
        if (any(AttsR.eq."trYMinF")) then
            locResR@xyComputeYMin = False  ; same
        end if                            
        if (any(AttsR.eq."trYMaxF")) then
            locResR@xyComputeYMax = False  ; same
        end if
    end if

    getvalues xyL               ; get assorted info from previous
      "vpHeightF"  : vpHeightF  ; plot (Left scale) for use Right scale
      "vpWidthF"   : vpWidthF
      "vpXF"       : vpXF
      "vpYF"       : vpYF
    end getvalues

    locResR@vpHeightF          = vpHeightF       ; set to same size
    locResR@vpWidthF           = vpWidthF 
    locResR@vpXF               = vpXF     
    locResR@vpYF               = vpYF     

    locResR@tiXAxisString      = ""     ; already done 

    locResR@tmXBLabelsOn       = False  ; Turn off bottom x axis labels
    locResR@tmXBMinorOn        = False  ; and tick marks.
    locResR@tmXBOn             = False

    locResR@tmXTLabelsOn       = False  ; Turn off top x axis labels
    locResR@tmXTMinorOn        = False  ; and tick marks.
    locResR@tmXTOn             = False

    locResR@tmYLLabelsOn       = False  ; Turn off left y axis labels
    locResR@tmYLMinorOn        = False  ; and tick marks.
    locResR@tmYLOn             = False

    locResR@tmYRLabelsOn       = True   ; Turn on right axis label
    locResR@tmYROn             = True   ; Turn on right tick marks
    locResR@tmYUseLeft         = False  
    locResR@tiYAxisSide        = "Right"; put title on Right scale

    locResR@gsnFrame           = False
    locResR@gsnDraw            = False 

    xyR = gsn_xy(wks,x,yR,locResR)  

    if (.not.any(ismissing(AttsR))) then 
        if (any(AttsR.eq."tmYRMode")) then
            setvalues xyR
              "tmYRMode"   : "Explicit"
              "tmYRValues" : locResR@tmYRValues 
              "tmYRLabels" : locResR@tmYRLabels 
            end setvalues
        end if
    end if

    anno = NhlAddAnnotation(xyL,xyR)
    setvalues anno
      "amZone"         : 0     ; Zone 0 centers tick marks over map.
      "amResizeNotify" : True  ; Resize tick marks if map resized.
    end setvalues
;    overlay (xyL,xyR)

    FRAME = True
    if (resL .and. isatt(resL,"gsnFrame") .and. .not.resL@gsnFrame) then
        FRAME = False
    end if
    DRAW = True
    if (resL .and. isatt(resL,"gsnDraw") .and. .not.resL@gsnDraw) then
        DRAW = False
    end if

    if (DRAW) then  
        draw (xyL)
    end if 
    if (FRAME) then
        frame (wks)
    end if 

    return ( xyL )
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

       if (isatt(plot,"contour")) then
           setvalues plot@contour
             "cnMonoLineThickness"    : False
             "cnLineThicknesses"      : cnlinethk
           end setvalues
       else
           setvalues plot
             "cnMonoLineThickness"    : False
             "cnLineThicknesses"      : cnlinethk
           end setvalues
       end if

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
    
     if (isatt(plot,"contour")) then
       setvalues plot@contour       
          "cnMonoLineDashPattern"  : False
          "cnLineDashPatterns"     : cnlinepat
	  "cnMonoLineColor"        : False
	  "cnLineColors"           : cncolor
        end setvalues
     else
        setvalues plot
          "cnMonoLineDashPattern"  : False
	  "cnLineDashPatterns"     : cnlinepat
	  "cnMonoLineColor"        : False
	  "cnLineColors"           : cncolor
        end setvalues
     end if
   end if                                ; any
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
     
       if (isatt(plot,"contour")) then
           setvalues plot@contour               ; Retrieve contour levels.
             "cnFillOn"          : True
             "cnMonoFillPattern" : True
             "cnMonoFillColor"   : False
             "cnFillColors"      : colist
           end setvalues
       else
           setvalues plot
             "cnFillOn"          : True
             "cnMonoFillPattern" : True
             "cnMonoFillCOlor"   : False
             "cnFillColors"      : colist
           end setvalues
       end if
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
