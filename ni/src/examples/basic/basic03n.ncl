; $Id: basic03n.ncl,v 1.7 1995-06-20 15:19:53 stautler Exp $
;
; This example demonstrates how to:
;   1. Create a scalar field data object and assign it to a plot.
;   2. Set resources using a resource file.
;   3. Set resources during object creation.
;   3. Set resources after object creation.

; Begin the ncl script.

begin

; ###########
; # FRAME 1 #
; ###########
; This frame demonstrates how to create and assign data to a contour plot.

; Create an X workstation.
 
wks = create "wks" windowWorkstationClass defaultapp
	"wkPause" : "True"
end create

; Create a 5x5 integer variable. 

data1=new((/5,5/), integer)

; Initialize the variable with data.

data1=(/(/3,4,4,5,5/), \
	(/2,3,5,5,4/), \
	(/2,4,5,4,4/), \
	(/3,4,4,4,3/), \
	(/3,3,3,3,3/)/)

; Create a scalar field object that will be used as a data set for a 
; contour object.  The sfDataArray resource is used to assign a data
; array to a scalar field data object.

field1 = create "field1" scalarFieldClass defaultapp
	"sfDataArray" : data1
end create

; Create a contour plot object and assign the data using the
; cnScalarFieldData resource.

con1 = create "con1" contourPlotClass wks
	"cnScalarFieldData" : field1
end create

; Draw the plot. 

draw(con1)

; Update and clear the workstation.

frame(wks)

; ###########
; # FRAME 2 #
; ###########
; This frame demonstrates how to set resources using a resource file.

; Resources are read from a resource file called basic03.res because
; the first argument in the create call is "basic03". This resource file
; is only read at the time an application object is created.
; The resource file contains resource assignments that control the
; characteristics of a plot.

appid = create "basic03" appClass defaultapp
end create

; Create another workstation window and make it a child of the
; new application object by using the appid variable as the argument
; for the parent id.  By making this a child of the application
; object, the resources that are set in the basic03.res resource
; file will apply to this object and its children.

wks2 = create "wks2" windowWorkstationClass appid
end create

; Create another contour plot object and assign the data.
; Notice that the parent id is wks2, making the contour object
; a child of the new workstation.

con2 = create "con2" contourPlotClass wks2
	"cnScalarFieldData" : field1
end create

; The contour object is drawn with filled contours because there is
; a resource in basic03.res that specifies that contour fill is on.

draw(con2)

; Updates and clear the workstation.

frame(wks2)

; ###########
; # FRAME 3 #
; ###########
; This frame demonstrates how resources can be set when an object is
; created.  

; A variable length list of resource name/value pairs specifies
; a resource and its value.  In this example contour line labels are turned
; off by setting the "cnLineLabelsOn" resource to "False".

con3 = create "con3" contourPlotClass wks2
	"cnScalarFieldData" : field1
	"cnLineLabelsOn"    : "False"
end create

; Draw the contour object.

draw (con3)

; Update and clear the workstation.

frame(wks2)

; ###########
; # FRAME 4 #
; ###########
; This frame demonstrates how to use the setvalues expression to set 
; resources for an object that has already been created.

; The setvalues expression is used to assign values to the resources
; of a object whose id is given as the first argument in the expression.
; In this example, that argument is "con3."
;
; Any resource that is valid for the con3 object can be set in the following
; expression.  In this example, setting "cnFillOn" to "False" turns 
; contour fill off.  By default, cnFillOn is "False", but since it
; is set to "True" in the resource file, we can override that value by
; using the setvalues expression.  

setvalues con3
	"cnFillOn"          : "False"
end setvalues

; Draw the contour object.

draw (con3)

; Update and clear the workstation

frame(wks2)


; Clean up (deleting the parent object recursively deletes all of its 
; children).

delete(wks)
delete(appid)

; End the ncl script.

end
