#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>

/*
** Define error file, Fortran unit number, and workstation type,
** and workstation ID.
*/
#define IERRF  "stdout"
#define LUNIT  "gmeta"
#define IWTYPE SED_WSTYPE
#define IWKID  1
#define ISZDM  0


int main()
{
      	extern void cmpusr();
/*
** Open GKS, and turn off clipping.
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
        gset_clip_ind(0);
/*
** Call the mapping routine cmpusr
*/
      	cmpusr();
/*
** Close GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
/*
** Done.
*/
        return(0);
}


void cmpusr()
/*
** Demo driver.
*/
{
	extern int mapusr_ (int*);	/* dash pattern selector */

/*
** Set up Satellite-view.
*/
      c_maproj ("SV",40.,10.,0.);
      c_mapstr ("SA - SATELLITE DISTANCE",5.);
/*
** Use country outline set
*/
      c_mapstc ("OU - OUTLINE DATASET SELECTOR","PO");
/*
** Set Dot Distance to be 1
*/
      c_mapstr ("DD - DISTANCE BETWEEN DOTS",1.);
 
      c_mapdrw();
      
      c_frame();
}
      
      
int mapusr_ (int* iprt)
{
/*
** mapusr_ is used to change the appearance of various parts of a map.
** It is called just before and just after each portion of a map is
** drawn.  The default version does nothing.  This version chooses a
** different dash line pattern for each portion of the map in a different
** dash pattern.
**
** iprt addresses each portion of the map as follows:
**   1 - Perimeter
**   2 - Grid
**   3 - Labels
**   4 - Limb lines
**   5 - Continental outlines
**   6 - US state outlines
**   7 - International outlines
*/

	int ipat;	/* dash pattern */

/*
** 1110000011100000
*/
	ipat = 57568;
      	if (*iprt == 1) c_dashdb(&ipat);
/*
** 1111111100000000 
*/
	ipat = 65280;
      	if (*iprt == 2) c_dashdb(&ipat);
/*
** 1111111111111111
*/
	ipat = 65535;
      	if (*iprt == 3) c_dashdb(&ipat);
/*
** 0100110001110000
*/
	ipat = 19568;
      	if (*iprt == 4) c_dashdb(&ipat);
/*
** 1111000011110000 
*/
	ipat = 61680;
      	if (*iprt == 5) c_dashdb(&ipat);
/*
** 1110010011100100
*/
	ipat = 58596;
      	if (*iprt == 6) c_dashdb(&ipat);
/*
** 010101010101010101
*/
	ipat = 21845;
      	if (*iprt == 7) c_dashdb(&ipat);

	return 1;
}
