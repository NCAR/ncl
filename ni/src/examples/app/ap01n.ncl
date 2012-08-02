;
;      $Id: ap01n.ncl,v 1.5 2010-03-15 15:45:19 haley Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1995                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       ap01n.ncl
;;
;;  Author:     Jeff Boote (converted to NCL by Mary Haley)
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Thu Sep 28 08:15:25 MDT 1995
;;
;;  Description:    This program shows use resource files in various ways.
;;

;
; Begin NCL script.
;
begin

;
; Create an App object so we can have an application specific
; resource file for this example.  Since the App object is the
; one that reads in the application specific resource files,
; these resources must be set programmatically.  (They could
; be set in the $(NCARG_SYSRESFILE) or $(NCARG_USRRESFILE), but
; these things are pretty specific to this example, so I am
; setting them programmatically.)
;
appid = create "ap01" appClass defaultapp
    "appDefaultParent" : "True"
    "appUsrDir" : "./"
end create

;
; Create the Workstation to manage the output device.
; Since the appDefaultParent resource was set to True for
; "ap01", we can use either the constant defaultapp or
; appid as the Parent id.  They mean the same thing.
;
; Default is to display output to an X workstation
;
wks_type = "x11"

if (str_lower(wks_type).eq."ncgm") then
;
; Create an NCGM workstation object.
;
  workid = create "x" ncgmWorkstationClass defaultapp
      "wkMetaName" : "ap01n.ncgm"
  end create
end if
if (str_lower(wks_type).eq."x11") then
;
; Create an X workstation object.
;
  workid = create "x" windowWorkstationClass defaultapp
    "wkPause" : "True"
  end create
end if
if (str_lower(wks_type).eq."oldps") then
;
; Create a PostScript workstation object.
;
  workid = create "x" psWorkstationClass defaultapp
    "wkPSFileName" : "ap01n.ps"
  end create
end if
if (str_lower(wks_type).eq."oldpdf") then
;
; Create a PDF workstation object.
;
  workid = create "x" pdfWorkstationClass defaultapp
    "wkPDFFileName" : "ap01n.pdf"
  end create
end if

if (str_lower(wks_type).eq."pdf".or.str_lower(wks_type).eq."ps") then
;
; Create a cairo PS or PDF workstation object.
;
  workid = create "x" documentWorkstationClass defaultapp
    "wkFileName" : "ap01n"
    "wkFormat"   : str_lower(wks_type)
  end create
end if

if (str_lower(wks_type).eq."png") then
;
; Create a cairo PNG workstation object.
;
  workid = create "x" imageWorkstationClass defaultapp
    "wkFileName" : "ap01n"
    "wkFormat"   : str_lower(wks_type)
  end create
end if

;
; Create a TextItem.  I am not programmatically setting any of
; the TextItem resources, so the Resource Database made up from
; the resource files read-in by the "ap01" App object is specifying
; all the attributes to the TextItem.
;
tx1 = create "tx1" textItemClass workid end create

;
; Call draw on the Workstation Object.  This will cause all of the
; Workstation's children to Draw.  In this case, this is only
; "tx1", so draw could have been called on it, just as easily.
;
draw(workid)

;
; Call frame on the Workstation Object.  This is functionally
; equivalent to calling NhlUpdateWorkstation, and then
; NhlClearWorkstation.
;
frame(workid)

;
; End NCL script.
;
end

