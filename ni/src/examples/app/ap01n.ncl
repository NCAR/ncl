;
;      $Id: ap01n.ncl,v 1.2 1997-09-30 16:26:39 haley Exp $
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
NCGM=0
X11=1
PS=0

if (NCGM .eq. 1) then
;
; Create an NCGM workstation object.
;
  workid = create "x" ncgmWorkstationClass defaultapp
      "wkMetaName" : "ap01n.ncgm"
  end create
else
  if (X11 .eq. 1) then
;
; Create an X workstation object.
;
    workid = create "x" xWorkstationClass defaultapp
      "wkPause" : "True"
    end create
  else
    if (PS .eq. 1) then
;
; Create a PostScript workstation object.
;
      workid = create "x" psWorkstationClass defaultapp
        "wkPSFileName" : "ap01n.ps"
      end create
    end if
  end if
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

