#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/TextItem.h>

main()
{
	int	appid, workid, textid;
	int	rlist;

	/*
	 * Initialize the HLU library.
	 */
	NhlInitialize();

	/*
	 * Create a ResList.  The ResList is a dynamic list of the resources
	 * and values being explicitly set during the "NhlCreate" function.
	 */
	rlist = NhlRLCreate(NhlSETRL);

	/*
	 * Create an App object so we can have an application specific
	 * resource file for this example.  Since the App object is the
	 * one that reads in the application specific resource files,
	 * these resources must be set programmatically.  (They could
	 * be set in the $(NCARG_SYSRESFILE) or $(NCARG_USRRESFILE), but
	 * these things are pretty specific to this example, so I am
	 * setting them programmatically.)
	 */
	NhlRLClear(rlist);
	NhlRLSetInteger(rlist,NhlNappDefaultParent,True);
	NhlRLSetString(rlist,NhlNappUsrDir,"./");
	NhlCreate(&appid,"ap01",NhlappLayerClass,NhlDEFAULT_APP,rlist);

	/*
	 * Destroy the ResList.
	 * Since I am not setting any more resources programmatically, I
	 * don't need the ResList anymore.
	 */
	NhlRLDestroy(rlist);

	/*
	 * Create the Workstation to manage the output device.
	 * Since the NhlNappDefaultParent resource was set to True for
	 * "ap01", we can use either the constant NhlDEFAULT_APP or
	 * appid as the Parent id.  They mean the same thing.
	 */
	NhlCreate(&workid,"x",NhlxWorkstationLayerClass,NhlDEFAULT_APP,0);

	/*
	 * Create a TextItem.  I am not programatically setting any of
	 * the TextItem resources, so the Resource Database made up from
	 * the resource files read-in by the "ap01" App object is specifying
	 * all the attributes to the TextItem.
	 */
	NhlCreate(&textid,"tx1",NhltextItemLayerClass,workid,0);

	/*
	 * Call draw on the Workstation Object.  This will cause all of the
	 * Workstation's children to Draw.  In this case, this is only
	 * "tx1", so draw could have been called on it, just as easily.
	 */
	NhlDraw(workid);

	/*
	 * Call frame on the Workstation Object.  This is functionally
	 * equivalent to calling NhlUpdateWorkstation, and then
	 * NhlClearWorkstation.
	 */
	NhlFrame(workid);

	/*
	 * Destroying an object also destroy's it's children, so by destroying
	 * "ap01", all of the objects get destroyed.
	 */
	NhlDestroy(appid);

	/*
	 * Close the HLU library.  This free's up most of the dynamic memory
	 * the HLU library allocates.
	 */
	NhlClose();

	exit(0);
}
