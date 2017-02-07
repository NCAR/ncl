;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_insert_logo.ncl
; 
;  This example requires the Climate Data Operators (cdo), which
;  can be downloaded from: https://code.zmaw.de/projects/cdo/files
;
;  KMF 
;-----------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
;-- generate dummy data
  x  = ispan(0,100,1)
  y  = cos(0.0628*ispan(0,100,1))     ;-- generate a curve with 101 points.

;-- output file
  dummy  = "dum_tmp.png"
  output = "NUG_insert_logo.png"
  
;-- open a workstation
  wks  = gsn_open_wks("png", dummy)

;-- set plot resources
  res                            =  True
  res@gsnDraw                    =  False       ;-- don't draw plot yet
  res@gsnFrame                   =  False       ;-- don't advance frame
  res@gsnYRefLine                =  0.0         ;-- create a reference line   
  res@gsnYRefLineThicknessF      =  4.0         ;-- create a reference line   

  res@xyLineThicknessF           =  4.0         ;-- line thickness
  
  res@tiMainString = "          Cosine function~C~y = cos(0.0628 * ispan(0,100,1))"    ;-- main title string
  res@tiXAxisString              = "x-axis"     ;-- set x-axis title string
  res@tiYAxisString              = "y-axis"     ;-- set x-axis title string
  res@tiMainOffsetYF             =  0.05        ;-- move title string
  res@tiMainFont                 = "times-roman" ;-- title string font size

  res@tmXBLabelFontHeightF       =  0.025       ;--larger x-label font size
  res@tmYLLabelFontHeightF       =  0.025       ;--larger y-label font size
  
;----------------------------------------------------------
;-- create the plot
;----------------------------------------------------------
  plot = gsn_csm_xy(wks, x, y, res)             ;-- create the default plot

;----------------------------------------------------------
;-- additional text on plot using plot coordinate
;----------------------------------------------------------
  txres                =  True           ;-- text resources additional text
  txres@txFontColor    = "blue"          ;-- change to white
  txres@txFontHeightF  =  0.015          ;-- decrease font size
  txres@txJust         = "CenterCenter"  ;-- text justification

  id = gsn_add_text(wks, plot, "maximum", 50, 0.99, txres)  
                                         ;-- center position x=50, y=0.99
  id = gsn_add_text(wks, plot, "minimum", 10, -0.99, txres) 
                                         ;-- center position x=10, y=-0.99
;----------------------------------------------------------
;-- draw red lines
;----------------------------------------------------------
  plres  =  True
  plres@gsLineColor      = "red"
  plres@gsLineThicknessF =  4.0
  
  plid1 = gsn_add_polyline(wks, plot, (/ 5,40/), (/0.99,0.99/), plres)
  plid2 = gsn_add_polyline(wks, plot, (/60,95/), (/0.99,0.99/), plres)
  plid3 = gsn_add_polyline(wks, plot, (/20,45/), (/-0.99,-0.99/), plres)

;----------------------------------------------------------
;-- additional text on plot using page coordinate (NDC)
;----------------------------------------------------------
  ndcres                =  True         ;-- text resources copyright string
  ndcres@txFontColor    = "green"            ;-- change to white
  ndcres@txFontHeightF  =  0.02              ;-- make font size smaller
  ndcres@txJust         = "CenterCenter"     ;-- text justification
  
  gsn_text_ndc(wks,"x-axis", 0.5, 0.83, ndcres) ;-- draw right axis label

  ndcres@txAngleF               = -90.          ;-- rotate the text

  gsn_text_ndc(wks,"y-axis", 0.84, 0.5, ndcres) ;-- draw right axis label

;----------------------------------------------------------
;-- draw the plot
;----------------------------------------------------------
  draw(plot)
  frame(wks)
  delete(wks)

;----------------------------------------------------------
;-- add a logo to the finished plot (upper left corner)
;-- (this could be done only for PNG plot output)
;----------------------------------------------------------

  logo = "./NCLLogoWeb.jpg"
  if (.not. fileexists(logo)) then
     print("")
     print("You don't have the necessary NCL logo for this script. You can download it from:​")
     print("")
     print("http://www.ncl.ucar.edu/Images/NCLLogoWeb.jpg")
     print("")
     print("or use the wget command:")
     print("")
     print("wget http://www.ncl.ucar.edu/Images/NCLLogoWeb.jpg")
     print("")
     exit
  end if
  

  cmd  = "composite -geometry 80x80+20+20 "+logo+" "+dummy+" "+output
  system(cmd)
  system("rm -rf "+dummy)

end
