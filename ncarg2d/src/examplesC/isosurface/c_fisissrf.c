/*
** FILE c_fisissrf.c
** 
** Simple demonstration of ISOSRF.
** This file is a direct translation of fisissrf.f.
*/
 
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

/*
** Demo driver 
*/
void tisosr(int);

int main()
{
	int ierr= 0;	/* error flag */

/*
** OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
*/
        gopen_gks(IERRF, ISZDM);
        gopen_ws(IWKID, LUNIT, IWTYPE);
        gactivate_ws(IWKID);
/*
** INVOKE DEMO DRIVER
*/
      	tisosr(ierr);
/*
** DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
*/
        gdeactivate_ws(IWKID);
        gclose_ws(IWKID);
        gclose_gks();
      	return 0;
}


void tisosr (int ierror)
{
/*
** PURPOSE                To provide a simple demonstration of ISOSRF.
**
** USAGE                  tisosr (ierror)
**
** ARGUMENTS
**
** ON OUTPUT              ierror
**                          An integer variable
**                          = 0, if the test was successful,
**                          = 1, the test was not successful.
**
** I/O                    If the test is successful, the message
**
**               ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY
**
**                        is printed on unit 6.  In addition, 2
**                        frames are produced on the machine graphics
**                        device.  In order to determine if the test
**                        was successful, it is necessary to examine
**                        the plots.
**
** PRECISION              Single
**
** LANGUAGE               C
**
** REQUIRED ROUTINES      ISOSRF
**
** REQUIRED GKS LEVEL     0A
**
** ALGORITHM              Values of a function on a 3-D rectangular grid
**                        are stored in array T.  Entries EZISOS and
**                        ISOSRF are called to draw iso-valued surface
**                        plots of the function.
*/

      	float t[19][31][21]; 	/* data array */
	float slab[33][33];	/* scratch array */
	float eye[3];		/* eye position in UVW coordinates */

	int i,j,k;		/* counters */

/*
** Specify coordinates for plot titles.
*/
      	float ix= 0.44;
	float iy= 0.95;
/*
** Data calculation variables
*/
	float f1, f2;
	float fimid, fjmid1, fjmid2, fkmid;
	float fip,fjp1,fjp2,fkp1;
	float temp;

       	float rbig1 = 6.0;
	float rbig2 = 6.0;
	float rsml1 = 2.0;
	float rsml2 = 2.0;
      	float tiso = 0.0;

	int mu,mv,mw;
	int jcent1, jcent2;

      	int muvwp2 = 33;
      	int iflag = -7;
      	int nu = 21;
	int nv = 31;
	int nw = 19;
/*
** Color representation structure
*/
        Gcolr_rep colr_rep;     /* RGB color values */
/*
** Initialize the error parameter.
*/
      	ierror = 1;
/*
** Background is White
*/
        colr_rep.rgb.red = 1.0;
        colr_rep.rgb.green = 1.0;
        colr_rep.rgb.blue = 1.0;
        gset_colr_rep (IWKID,0,&colr_rep);
/*
** Foreground is black
*/
        colr_rep.rgb.red = 0.0;
        colr_rep.rgb.green = 0.0;
        colr_rep.rgb.blue = 0.0;
        gset_colr_rep (IWKID,1,&colr_rep);
/*
** Fill the 3-D array to be plotted.
*/
      	jcent1 = nv * 0.5 - rbig1 * 0.5;
      	jcent2 = nv * 0.5 + rbig2 * 0.5;
	for(i=0; i<nu; ++i)
	{
         	fimid = i + 1 - (nu / 2);
		for(j=0; j<nv; ++j)
		{
            		fjmid1 = j + 1 - jcent1;
            		fjmid2 = j + 1 - jcent2;
			for(k=0; k<nw; ++k)
			{
               			fkmid = k + 1 - nw / 2;
               			f1 = sqrt(rbig1*rbig1/
					 (fjmid1*fjmid1+fkmid*fkmid+.1));
               			f2 = sqrt(rbig2*rbig2/
					 (fimid*fimid+fjmid2*fjmid2+.1));
               			fip = (1.-f2)*fimid;
               			fjp1 = (1.-f1)*fjmid1;
               			fjp2 = (1.-f2)*fjmid2;
               			fkp1 = (1.-f1)*fkmid;
               			t[k][j][i] = fimid*fimid+fjp1*fjp1+fkp1*fkp1-
                          		     rsml1*rsml1;
                            	temp= fkmid*fkmid+fip*fip+fjp2*fjp2-
                                             rsml2*rsml2;
				if (temp < t[k][j][i]) t[k][j][i] = temp;
			}
		}
	}	
/*
** Define the eye position.
*/
      	eye[0] = 100.0;
      	eye[1] = 150.0;
      	eye[2] = 125.0;
/*
**
**     Frame 1 -- The EZISOS entry.
**
** Select normalization transformation 0.
*/
	gsel_norm_tran(0);
/*
** Call PLCHLQ to write the plot title.
*/
      	c_pcloqu(ix,iy,"DEMONSTRATION PLOT FOR ENTRY EZISOS OF ISOSRF",
                 16.,0.,0.);
     
      	c_ezisos(&t[0][0][0],nu,nv,nw,eye,&slab[0][0],tiso);
/*
**
**     Frame 2 -- The ISOSRF entry.
**
** Select normalization transformation 0.
*/
	gsel_norm_tran(0);
/*
** Call PLCHLQ to write the plot title.
*/
      	c_pcloqu(ix,iy,"DEMONSTRATION PLOT FOR ENTRY ISOSRF OF ISOSRF",
                 16.,0.,0.);
/*
** Test ISOSRF with a subarray of T.
*/
      	mu = nu/2;
      	mv = nv/2;
      	mw = nw/2; 
      	muvwp2 = mu;
        if (mv > muvwp2) muvwp2 = mv;
        if (mw > muvwp2) muvwp2 = mw;
      	muvwp2 = muvwp2 + 2;
      	c_isosrf(&t[mw-1][mv-1][mu-1],nu,mu,nv,mv,mw,eye,muvwp2,
                 &slab[0][0],tiso,iflag);
      	c_frame();
 
	if (printf(" ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY\n"))
      		ierror = 0;
}
