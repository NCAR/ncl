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
;                      out of NCGM, PDF, PostScript, and X11.
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
; Create an older-style PostScript workstation.
;
	 widp = create "basic07ps" psWorkstationClass defaultapp
		 "wkPSFileName": "basic07n.ps"
		 "wkOrientation": "portrait"
		 "wkPSFormat": "ps"
         end create	
;
; Create an older-style PDF workstation.
;
	 widpdf = create "basic07pdf" pdfWorkstationClass defaultapp
		 "wkPDFFileName": "basic07n.pdf"
		 "wkOrientation": "portrait"
		 "wkPDFFormat": "pdf"
         end create	
;
; Create an X Workstation.
;
	 widx = create "basic07x11" windowWorkstationClass defaultapp
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
      
      pidpdf = create "TextItems" textItemClass widpdf
	      "txBackgroundFillColor": i
      end create

      draw(pidx)
      draw(pidn)
      draw(pidp)
      draw(pidpdf)
      frame(widx)
      frame(widp)
      frame(widpdf)
      frame(widn)
 
      destroy(pidx)
      destroy(pidn)
      destroy(pidp)
      destroy(pidpdf)
      destroy(widx)
      destroy(widp)
      destroy(widpdf)
      destroy(widn)
      destroy(appid)

end
