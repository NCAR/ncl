;-----------------------------------------------------------------
;  NCL User Guide Example:  NUG_text_settings.ncl
; 
;  KMF 30.10.14
;-----------------------------------------------------------------
; This load command is not required in NCL versions 6.2.0 and later.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin
;-- define german "Umlaute"
  Auml    = "A~H-15V6F35~H~FV-6H3~"
  auml    = "a~H-13V2F35~H~FV-2H3~"
  Ouml    = "O~H-16V6F35~H~FV-6H3~"
  ouml    = "o~H-14V2F35~H~FV-2H3~"
  Uuml    = "U~H-15V6F35~H~FV-6H3~"
  uuml    = "u~H-13V2F35~H~FV-2H3~"

;-- define super- and subscript variable
  super   = "m~S~3~N~  [m s~S~-1~N~]  [kg m~S~-2~N~]  30~S~o~N~C"
  sub     = "Schwefels"+auml+"ure:   H~B~2~N~SO~B~4~N~"

  data  = (/ "Data-1",   "Data-2", "Data-3",  "Data-4", "Data-5"/)
  diff  = (/ 16.25,    -0.93,    0.43,     3.5,   0.0/)
  var   = (/  0.06,     0.02,    0.04,     0.05,  0.03/)
  ratio = (/     2,      2.4,     1.1,     0.9,   0.0/)

  ntext = dimsizes(data)

;-- open workstation
  wks = gsn_open_wks("png","NUG_text_settings")   

;-- x, y start point for writing
  x   = 0.1
  y   = 0.95
  inc = 0.07

;-- text resources
  txres                   = True   
  txres@txFontHeightF     = 0.03
  txres@txJust            = "CenterCenter"
  str                     = "NCL Doc Example: Text settings"
  gsn_text_ndc(wks,str,0.5,y,txres) 

  txres@txJust            = "CenterLeft"
  str1                    = "Umlaute:"
  gsn_text_ndc(wks,str1,x,y-2*inc,txres)

  str2                    = Auml+"  "+auml+"   "+Ouml+"  "+ouml+"   "+Uuml+" "+uuml
  gsn_text_ndc(wks,str2,x+0.3,y-2*inc,txres)

  str1                    = "Superscript:"
  gsn_text_ndc(wks,str1,x,y-3*inc,txres)
  str2                    = super
  gsn_text_ndc(wks,str2,x+0.3,y-3*inc,txres)

  str1                    = "Subscript:"
  gsn_text_ndc(wks,str1,x,y-4*inc,txres)
  str2                    = sub
  gsn_text_ndc(wks,str2,x+0.3,y-4*inc,txres)

;-- nice formated text output using sprintf
  str = "Format:"
  gsn_text_ndc(wks,str,x,y-5*inc,txres)

  xpos = 0.4
  do i=0,ntext-1
    ypos = y-5*inc-i*0.05
    gsn_text_ndc(wks,data(i),xpos,ypos,txres)
  end do

  txres@txJust = "CenterRight"
  do i=0,ntext-1
    xpos = 0.65
    ypos = y-5*inc-i*0.05
    if(diff(i).ne.0.0) then
      str     = sprintf("%6.2f",diff(i))
      gsn_text_ndc(wks,str,xpos,ypos,txres)
    else
      str     = "-"
      gsn_text_ndc(wks,str,xpos,ypos,txres)
    end if
    xpos = xpos + 0.12
    if(var(i).ne.0.0) then
      str     = sprintf("%5.2f",var(i))
      gsn_text_ndc(wks,str,xpos,ypos,txres)
    end if
    xpos = xpos + 0.12
    if(ratio(i).ne.0.0) then
      str     = sprintf("%3.1f",ratio(i))
      gsn_text_ndc(wks,str,xpos,ypos,txres)
    else
      str     = "-"
      gsn_text_ndc(wks,str,xpos,ypos,txres)
    end if
  end do

;-- greek characters
  xpos = 0.3
  ypos = 0.3
  str1                    = "Greek font:"
  gsn_text_ndc(wks,str1,xpos,ypos,txres)
  str2                    = "alpha = ~F33~a~N~"
  gsn_text_ndc(wks,str2,xpos+0.27,ypos,txres)
  str2                    = "beta   = ~F33~b~N~"
  gsn_text_ndc(wks,str2,xpos+0.27,ypos-0.05,txres)
  str2                    = "sigma = ~F33~s~N~"
  gsn_text_ndc(wks,str2,xpos+0.28,ypos-0.10,txres)

;-- decrease the font
  str1                    = "Font size 100%"
  gsn_text_ndc(wks,str1,xpos+0.08,ypos-3*inc,txres)
  str2                    = "~Z70~Font size 70%~N~"
  gsn_text_ndc(wks,str2,xpos+0.3,ypos-3*inc,txres)
  str3                    = "~Z40~Font size 40%~N~"
  gsn_text_ndc(wks,str3,xpos+0.45,ypos-3*inc,txres)

  frame(wks)
end
