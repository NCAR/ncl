load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

undef("addcyclic")
function addcyclic(data[*][*]:float)
;
; Add a cyclic point in "x" to a 2D array
; for a lat/lon plot "x"  corresponds to "lon"
;                    "ny" corresponds to "nlat"
;                    "mx" corresponds to "mlon"
local dims, newdata, ny, mx, mx1
begin
    dims    = dimsizes(data)
    ny      = dims(0)
    mx      = dims(1)
    mx1     = mx+1

    newdata = new((/ny  ,mx1/),typeof(data))

    newdata(:,0:mx-1) = data             ; pass everything
    newdata(:,mx)     = (/ data(:,0) /)  ; value only

    if(isdimnamed(newdata,1) .and. iscoord(data,newdata!1)) then 
        newdata&$newdata!1$(mx) = newdata&$newdata!1$(0)
    end if
 
    return(newdata)
end
 
begin
  wks = gsn_open_wks("x11","gsun11n")  ; Open an X11 workstation.

  cmap = (/(/1.,1.,1./), (/0.,0.,0./)/)  ; Change background to white
  gsn_define_colormap(wks,cmap)          ; and foreground to black.

  data     = asciiread("u.cocos",(/39,14/),"float")

  pressure  = data(:,0)    ; First column of data is pressure (mb).
  height    = data(:,1)    ; Second column is height (km).
  u         = data(:,2:13) ; Rest of columns are climatological zonal winds
                           ; (u: m/s)
  unew = addcyclic(u)  ; Copy first point to end of array for continuity.

;----------- Begin first plot -----------------------------------------

  resources = True

  resources@tiMainString   = "~F26~Cocos Island"   ; Main title.
  resources@tiYAxisString  = "~F25~Pressure (mb)"  ; Y axes label.

  resources@sfYCStartV = max(pressure)   ; Indicate start and end of left
  resources@sfYCEndV   = min(pressure)   ; Y axes values.

  resources@trYReverse  = True    ; Reverse the Y values.
  resources@trYLog      = True    ; Use log scale.

  resources@tmXBMode      = "Explicit"   ; Define your own tick mark labels.
  resources@tmXBLabelFont = 25           ; Change font of labels.
  resources@tmXBLabelFontHeightF = 0.015 ; Change font height of labels.
  resources@tmXBMinorOn   = False        ; No minor tick marks.
  resources@tmXBValues    = ispan(0,12,1); Location to put tick mark labels
                                         ; (13 points with January repeated).
  resources@tmXBLabels    = (/"Jan","Feb","Mar","Apr","May","Jun",\
                              "Jul","Aug","Sep","Oct","Nov","Dec","Jan"/)

  resources@tmYUseLeft    = False      ; Keep right axis independent of left.
  resources@tmYRLabelsOn  = True       ; Turn on right axis labels.
  resources@tmYRLabelFont = 25         ; Change font of labels.
  resources@tmYROn        = True       ; Turn on right axis tick marks.
  resources@tmYRMinorOn   = False      ; No minor tick marks.

  resources@tmYRMode      = "Explicit"  ; Define own tick mark labels.
  hnice = fspan(0.,22.,12)              ; Set range of "nice" height values.
  pnice = ftcurv(height,pressure,hnice) ; Calculate "nice" pressure values.
  resources@tmYRValues    = pnice       ; At each "nice" pressure value, 
  resources@tmYRLabels    = hnice       ; put a "height" value label.

  resources@tmYLMode      = "Explicit" ; Define own tick mark labels.
  resources@tmYLLabelFont = 25         ; Change the font.
  resources@tmYLValues    = (/1000.,  800., 700., 500., 400., 300., \
                               250.,  200., 150., 100.,  50./)
  resources@tmYLLabels    = (/"1000","800","700","500","400","300", \
                               "250","200","150","100", "50"/)
  resources@tmYLMinorOn   = False        ; No minor tick marks.

  resources@cnFillOn          = True  ; Turn on contour level fill.
  resources@cnMonoFillColor   = True  ; Use one fill color.
  resources@cnMonoFillPattern = False ; Use multiple fill patterns.

  resources@cnLineLabelAngleF = 0. ; Draw contour line labels right-side up.
  resources@cnLevelSpacingF   = 1.0

  resources@gsnDraw  = False  ; Don't draw the plot or advance the
  resources@gsnFrame = False  ; frame in the call to gsn_contour.

  contour = gsn_contour(wks, unew, resources)  ; Create a contour plot.

  getvalues contour           ; Retrieve contour levels.
    "cnLevels"   : levels
  end getvalues

  patterns  = new(dimsizes(levels)+1,integer) ; Create array for fill 
  patterns(:)  = -1 	                      ; patterns and initialize
                                              ; it to transparent.
  
  do i=0,dimsizes(levels)-1         ; Fill contour levels depending on 
    if(levels(i).le.-6.) then       ; different criteria.
      patterns(i) = 5               ; Hatching
    else
      if(levels(i).gt.0.) then
        patterns(i) = 17            ; Stipple
      end if
    end if
  end do
  patterns(i)  = 17 ; last pattern

  setvalues contour    ; Set the new patterns and make them more dense.
    "cnFillPatterns" : patterns
    "cnFillScaleF"   : 0.8
  end setvalues

  draw(contour)  ; Draw the contour plot.

  txres               = True    ; Annotate plot with some text.
  txres@txFontHeightF = 0.015
  gsn_text_ndc(wks,"~F25~U Component",  .270,.815,txres)
  gsn_text_ndc(wks,"~F25~(m-s~S~-1~N~)",.765,.815,txres)


  txres@txFontHeightF = 0.025   ; Label right Y axis.
  txres@txAngleF      = 90.
  gsn_text_ndc(wks,"~F25~Height (km)",.89,.5,txres)

  frame(wks) ; Advance the frame.

;----------- Begin second plot -----------------------------------------

  delete(resources) ; Start with new list of resources.

  resources = True
  resources@tiMainString  = "~F26~Cocos Island"
  resources@tiXAxisString = "~F25~Month"
  resources@tiYAxisString = "~F25~Amplitude (m/s)"

  resources@tmXBMode      = "Explicit"   ; Define your own tick mark labels.
  resources@tmXBLabelFont = 25           ; Change font of labels.
  resources@tmXBLabelFontHeightF = 0.015 ; Change font height of labels.
  resources@tmXBMinorOn   = False        ; No minor tick marks.
  resources@tmXBValues    = ispan(0,12,1); Values from 0 to 12.
  resources@tmXBLabels    = (/"Jan","Feb","Mar","Apr","May","Jun",\
                              "Jul","Aug","Sep","Oct","Nov","Dec","Jan"/)

  xy = gsn_xy(wks,ispan(0,12,1),unew,resources) ; Create and draw an XY plot.

end

