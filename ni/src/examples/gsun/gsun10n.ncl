load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

undef("wigley")
function wigley(time:integer)
local i, j, k
begin

  y = new(dimsizes(time),float)

  i    = ind(time.lt.1953.)   ; Get indices where time is < 1953.
  y(i) = ((time(i)-1860.)/(1953.-1860.)) * 35.0

  j    = ind(time.ge.1953.and.time.le.1973) ; Indices where 1953 <= time < 1973
  y(j) = ((time(j)-1953.)/(1973.-1953.)) * (68. - 35.) + 35.

  k    = ind(time.gt.1973.and.time.le.1990) ; Indices where 1973 < time <- 1990.
  y(k) = ((time(k)-1973.)/(1990.-1973.)) * (75. - 68.) + 68.
  return(y)

end

begin

  time1 = (/  1990,  1985,  1980,  1970,  1960,  1950,  1940,  1930, \
              1920,  1910,  1900,  1890,  1880,  1870,  1860/)

  y1    = (/68.065, 65.00, 70.67, 63.06, 43.42, 28.28, 23.00, 20.25, \
             17.77, 15.36, 10.01,  6.40,  3.98,  2.18,  1.54/)

  time2 = ispan(min(time1),max(time1),1) ; Get range of equally space values.
  y2    = wigley(time2)                  ; Calculate proposed values as
                                         ; a function of time.
  maxdim = max( (/dimsizes(y1),dimsizes(y2)/) )

  y    = new((/2,maxdim/),float,-999.) ; Create new 2D arrays to hold 1D
  time = new((/2,maxdim/),float,-999.) ; arrays you just declared above.

  y(0,0:dimsizes(y1)-1) = y1  ; Copy y1 and y2 to y.
  y(1,0:dimsizes(y2)-1) = y2

  time(0,0:dimsizes(time1)-1) = time1  ; Copy time1 and time2 to time.
  time(1,0:dimsizes(time2)-1) = time2

  wks   = gsn_open_wks("x11","gsun10n") ; Open an X11 workstation.

  cmap = (/(/1.,1.,1./),(/0.,0.,0./)/)  ; Change background color to white
  gsn_define_colormap(wks,cmap)         ; and foreground color to black.

  resources              = True ; Get ready to set some resources.

  resources@vpWidthF     = 0.8
  resources@vpXF         = 0.13

  resources@tiMainString  = "~F22~Sulfur Emissions" ; "~F22~" changes
  resources@tiXAxisString = "~F22~Year"             ; the font to "22"
  resources@tiYAxisString = "~F22~Tg s/yr"          ; which is helvetica
                                                    ; bold.
  resources@tmXBLabelFont = 21
  resources@tmYLLabelFont = 21

  resources@trXMinF              = 1855 ; Set minimum X axes value.

  resources@xyDashPatterns       = (/16,0/)   ; ( dash, solid )
  resources@xyMarkLineModes      = (/"MarkLines","Lines"/)
  resources@xyMarker             = 1
  resources@xyMarkerSizeF        = 0.05 ; Default is 0.01

  resources@gsnFrame             = False ; Don't advance the frame.
  resources@gsnScale             = True  ; Draw X/Y axes labels in same size.

  xy = gsn_xy(wks,time,y,resources)  ; Create and draw XY plot.

  txresources               = True
  txresources@txFontHeightF = 0.015
  txresources@txJust        = "CenterLeft" ; Default is "CenterCenter".
  
  strings = (/"Wigley (Moller/IPCC)",\
         "~F22~CSM-proposed:~F~~C~(Orn et.al./GEIA + Smith)",\
         "~F22~CSM SO~B~4~N~ Scaling Factor: ~V1Q~~F22~S~B~emis~N~ (yr)~H-7Q~~V-1Q~---------------~H-9Q~~V-1Q~S~B~emis~N~ (1985)"/)

  xpos = (/1885.,1940.,1860./)   ; Define X/Y locations for text.
  ypos = (/30.,18.,70./)

  do i = 0,dimsizes(strings)-1  ; Loop through text strings and draw them.
    gsn_text(wks,xy,strings(i),xpos(i),ypos(i),txresources)
  end do

  frame(wks)  ; Advance the frame.
end
