/*
 *	$Id: scroll.c,v 1.6 1992-08-12 21:41:57 clyne Exp $
 */
/*
 *	scroll.c
 *
 *	Author		John Clyne
 *
 *	Date		Fri Oct 19 12:58:42 MDT 1990
 *
 *	The module contains widget independent code used by the scroll
 *	widget. 
 */
#include <sys/types.h>
#include "commands.h"
#include "display.h"
#include "bsd-sysv.h"

/*
 *	ScrollTo
 *	[exported]
 *
 *	This routine is called in response to the scroll bar being scrolled.
 *	It determines whether the scroll bar has moved sufficiently to warrant
 *	generating a plot command to a translator
 *
 * on entry
 *	id		: id of translator scrolled
 *	percent		: percent of scroll bar thumb wheel was moved to
 */
void	ScrollTo(id, percent)
	int	id;
	float	percent;
{
	int	start, stop;
	int	frame;
	char	buf[10];

	void	Command2();
	char	*GetValue();

	Command_Id	command_id;

	static	int	last_frame = 0;	/* kludge kludge	*/

	command_id.id = id;
	command_id.command	= GOTO;


	/*
	 * find out how many frames are covered by the segment defined
	 * for this translator
	 */
	start = atoi(GetValue(id, START_SEGMENT));
	stop = atoi(GetValue(id, STOP_SEGMENT));

	/*
	 * map percent into a frame number
	 */
	frame = (int) ((float) (stop - start + 1) * percent) + start;

	/*
	 * this is a hack. if the last frame from *any* translator matches the
	 * current requested frame do nothing
	 */
	if (frame == last_frame)
		return;	/* do nothing	*/

	last_frame = frame;	/* record current frame	*/

	/*
	 * send the plotting command
	 */
	(void) sprintf(buf, "%d", frame);
	Command((caddr_t) &command_id, GOTO_STRING, buf);

	/*
	 * update the displayed frame count
	 */
	UpdateFrameLabel(id, buf);
}
	
