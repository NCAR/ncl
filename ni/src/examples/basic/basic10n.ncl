;
; $Id: basic10n.ncl,v 1.2 1998-10-14 23:25:41 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                       ;
;                Copyright (C)  1998                                    ;
;        University Corporation for Atmospheric Research                ;
;                All Rights Reserved                                    ;
;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   File:       basic10n.ncl
;
;   Author: Mary Haley
;           National Center for Atmospheric Research
;           PO 3000, Boulder, Colorado
;
;
;   Date: Fri Oct  9 10:39:32 MDT 1998
;
;   Description: This example displays a named color map.
;

begin
;
; Open the netCDF file that contains the named colors.
;
    color_file = addfile("$NCARG_ROOT/lib/ncarg/data/cdf/color.nc","r")
    title = color_file@title                ; Title
    cmap = color_file->cmap_x               ; RGB values for color map
    ncolors = dimsizes(cmap(:,0))

;
; The attributes of cmap are the color names.  There are two names for
; each color, a lowercase name and a mixed case name.
;
    cmap_atts = onedtond(getvaratts(cmap),(/ncolors,2/))

;
; Set the display. Default is to display output to an X workstation
;
    NCGM=0
    X11=1
    PS=0

    if (NCGM .eq. 1) then
;
; Create an ncgmWorkstation object.
;
      wid = create "wks" ncgmWorkstationClass defaultapp
          "wkMetaName" : "basic10n.ncgm"
          "wkColorMap" : cmap
      end create
    else
      if (X11 .eq. 1) then
;
; Create an XWorkstation object.
;
        wid = create "wks" xWorkstationClass defaultapp
          "wkPause" : "True"
          "wkColorMap" : cmap
        end create
      else
        if (PS .eq. 1) then
;
; Create a PSWorkstation object.
;
          wid = create "wks" psWorkstationClass defaultapp
            "wkPSFileName" : "basic10n.ps"
            "wkColorMap" : cmap
          end create
        end if
      end if
    end if

;
; Create a textitem for drawing each label.
;	
    label = create "label" textItemClass wid
        "txFontHeightF" :  0.014
        "txFont"        : "helvetica-bold"
        "txJust"        : "CenterLeft"
    end create

;
; Create a textitem for the title.
;	
    txid = create "title" textItemClass wid
        "txFontColor"   : cmap@black
        "txPosXF"       : 0.5
        "txPosYF"       : 0.95
        "txFontHeightF" :  0.02
        "txFont"        : "helvetica-bold"
        "txString"      : "Named color map (lower case names may also be used)"
    end create
;
; X and Y positions of text in the view port. There will be
; 18 rows and 4 columns.
;
    xpos = fspan(.03,.81,4)
    ypos = fspan(.87,.07,18)

;
; 72 total colors per page.
;
    do k = 1,ncolors,72
      draw(txid)
      ii = 0
;
; Four color labels per line
;
      do i=k,k+71,4
        jj = 0
        do j = i,min((/i+3,ncolors/))
          Name = cmap_atts(j-1,0)           ; Get mixed case color name.
          name = cmap_atts(j-1,1)           ; Get lowercase color name.
;
; If color is black, then draw the label in white, otherwise draw
; the label in black.
;
          if(name.eq."black")
            setvalues label            
                "txFontColor" :  cmap@white  ; Draw label in white.
            end setvalues
          else   
            setvalues label            
                "txFontColor" :  cmap@black  ; Draw label in white.
            end setvalues
          end if
;
; Draw label using its color as the background color of the label box.
;
            setvalues label
                "txBackgroundFillColor" : cmap@$name$
                "txPosXF"               : xpos(jj)
                "txPosYF"               : ypos(ii)
                "txString"              : Name
            end setvalues
            draw(label)
          jj = jj + 1
        end do
        ii = ii + 1
      end do
      frame(wid)   ; Advance the frame.
   end do
end
