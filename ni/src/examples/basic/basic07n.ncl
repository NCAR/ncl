;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                all rights reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;      File:           basic07n.ncl
;
;      Author:         Bob Lackman 
;          National Center for Atmospheric Research
;          PO 3000, Boulder, Colorado
;
;      Date:           Fri May 25 18:31:18 mdt 1995
;
;      Description:    Demonstrates creating 3 simultaneous workstations.
;                      The TextItem output states which type of workstation,
;                      out of NCGM, PostScript, and X11.
;

begin

;
; Call a subroutine that generates an LLU
;
; Default is to create a metafile.
;

;
; Create an application context. Set the app dir to the current
; directory so the application looks for a resource file in the
; working directory. In this example the resource file supplies the
; plot title only.
;
      appid = create "basic07" appClass defaultapp
	      "appUsrDir": "./"
      	      "appDefaultParent": "True"
      end create	

;
; Create an NCGM workstation.
;
	 widn = create "basic07ncgm" ncgmWorkstationClass defaultapp
	 	"wkMetaName": "basic07n.ncgm"
         end create	

;
; Create a PostScript workstation.
;
	 widp = create "basic07ps" psWorkstationClass defaultapp
		 "wkPSFileName": "basic07n.ps"
		 "wkOrientation": "portrait"
		 "wkPSFormat": "ps"
         end create	
;
; Create an X Workstation.
;
	 widx = create "basic07x11" xWorkstationClass defaultapp
		 "wkPause": "True"
         end create	

;
; Create three plots, one for each workstation type.
;
;  Use color index 2
;
      i = 2
      pidx = create "TextItems" textItemClass widx
	      "txBackgroundFillColor": i
      end create

      pidn = create "TextItems" textItemClass widn
	      "txBackgroundFillColor": i
      end create

      pidp = create "TextItems" textItemClass widp
	      "txBackgroundFillColor": i
      end create

      draw(pidx)
      draw(pidn)
      draw(pidp)
      frame(widx)
      frame(widp)
      frame(widn)
 
      destroy(pidx)
      destroy(pidn)
      destroy(pidp)
      destroy(widx)
      destroy(widp)
      destroy(widn)
      destroy(appid)

end
