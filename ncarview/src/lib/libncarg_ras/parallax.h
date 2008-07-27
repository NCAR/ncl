/*
 *	$Id: parallax.h,v 1.8 2008-07-27 03:22:41 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	parallax.h
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	10/4/90
 *
 *	Description:
 *		This is the header file "parallax.h", which contains
 *		structures and macros for the Parallax VideoView
 *		video frame buffer. Access to the board is
 *		accomplished through memory-mapped I/O and ioctl()
 *		calls.
 *
 *		(Parallax distributes some code to be used with
 *		this board but the last time I reviewed it, it
 *		was almost entirely incorrect.)
 */

#define	VFB_WIDTH		640
#define	VFB_HEIGHT		512
#define VFB_Y_OFFSET		29
#define	VFB_CP_WIDTH		2048
#define	VFB_CP_HEIGHT		1024
#define VFB_VME_ADDRESS		0xa000000
#define VFB_DEVICE		"/dev/tvone0"
#define VFB_VME_DEVICE		"/dev/vme32d32"

char	*format_names[]	= { "NTSC", "YC", "YUV", "RGB" };

char	*out_names[]	= { "UNKNOWN", "ON SCREEN", "VIDEO OUT" };

char	*on_off[]	= { "OFF", "ON" };

char	*chroma_names[]	= { "AUTO", "ON", "OFF" };

struct f_fb_rgb {
	unsigned char unused;
	unsigned char b;
	unsigned char g;
	unsigned char r;
};

union f_fb_pixel {
	struct f_fb_rgb rgb;
	unsigned long lwd;
};

union f_cp_pixel {
	unsigned char byte[4];
	unsigned long lwd;
};

typedef struct FrameBufferStruct {
	struct {
		union f_fb_pixel pixel[1024];
	} line[VFB_HEIGHT];
} FrameBuffer;

typedef struct ControlPlaneStruct {
	struct {
		union f_cp_pixel pixel[VFB_CP_WIDTH/32];
	} cline[VFB_CP_HEIGHT];
} ControlPlane;

typedef struct InputRegisterStruct {
	unsigned int	unused:16,
			ibstate:1,
			abstate:3,
			chroma:2,
			filter:2,
			format:2,
			sync:3,
			compress:3;
} InputRegister;

/*reversed bit order from what it was J.K. */
typedef struct GeneralRegisterStruct {
	unsigned int	junk:8,
			jumpers:3,
			digitize_in_progress:1,	/* read-only */
			burst_absent:1, /* read-only */
			sync_absent:1,	/* read-only */
			ibstate:2,	/* input buffer state, read-only */
			nvclock:1,
			nvdata:2,
			ie1:1,
			ie0:1,
			showup:1,
			show525:1,
			live525:1,
			magic:8;
} GeneralRegister;

typedef struct OutputRegisterStruct {
	unsigned int	reserved:6,
			genlock:1,
			yuv:1,
			expand:1,
			yorg:12,
			xorg:11;
} OutputRegister;

typedef struct ControlRegistersStruct {
	unsigned long dac[4];
	union {
		unsigned long		lwd;
		InputRegister		bits;
	} in;
	unsigned long junka[3];
	union {
		unsigned long		lwd;
		OutputRegister		bits;
	} out;
	unsigned long junkb[3];
	union {
		unsigned long		lwd;
		GeneralRegister		bits;
	} gen;
} ControlRegisters;

typedef struct ParallaxStruct {
	FrameBuffer		fb;
	ControlPlane		cp;
	unsigned char		junk2[768*1024];
	ControlRegisters	regs;
} Parallax;
