#define playback_width 32
#define playback_height 32
#ifdef	IDT
char playback_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x80,
   0x00, 0xe0, 0x00, 0xe0, 0x00, 0xf8, 0x00, 0xf8, 0x00, 0xfe, 0x00, 0xfe,
   0x80, 0xff, 0x80, 0xff, 0xe0, 0xff, 0xe0, 0xff, 0xf8, 0xff, 0xf8, 0xff,
   0xfe, 0xff, 0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfc, 0xff, 0xfc, 0xff,
   0xf0, 0xff, 0xf0, 0xff, 0xc0, 0xff, 0xc0, 0xff, 0x00, 0xff, 0x00, 0xff,
   0x00, 0xfc, 0x00, 0xfc, 0x00, 0xf0, 0x00, 0xf0, 0x00, 0xc0, 0x00, 0xc0,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#else
extern	char playback_bits[];
#endif

#define jogback_width 32
#define jogback_height 32

#ifdef	IDT
char jogback_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00,
   0x00, 0x00, 0xe0, 0x00, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00, 0xfe, 0x00,
   0x00, 0x80, 0xff, 0x00, 0x00, 0xe0, 0xff, 0x00, 0x00, 0xf8, 0xff, 0x00,
   0x00, 0xfe, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xfc, 0xff, 0x00,
   0x00, 0xf0, 0xff, 0x00, 0x00, 0xc0, 0xff, 0x00, 0x00, 0x00, 0xff, 0x00,
   0x00, 0x00, 0xfc, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00, 0xc0, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#else
extern	char jogback_bits[];
#endif

#define stop_width 32
#define stop_height 32

#ifdef	IDT
char stop_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#else
extern	char stop_bits[];
#endif

#define jog_width 32
#define jog_height 32

#ifdef	IDT
char jog_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
   0x00, 0x07, 0x00, 0x00, 0x00, 0x1f, 0x00, 0x00, 0x00, 0x7f, 0x00, 0x00,
   0x00, 0xff, 0x01, 0x00, 0x00, 0xff, 0x07, 0x00, 0x00, 0xff, 0x1f, 0x00,
   0x00, 0xff, 0x7f, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0xff, 0x3f, 0x00,
   0x00, 0xff, 0x0f, 0x00, 0x00, 0xff, 0x03, 0x00, 0x00, 0xff, 0x00, 0x00,
   0x00, 0x3f, 0x00, 0x00, 0x00, 0x0f, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#else
extern	char jog_bits[];
#endif

#define play_width 32
#define play_height 32

#ifdef	IDT
char play_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00,
   0x07, 0x00, 0x07, 0x00, 0x1f, 0x00, 0x1f, 0x00, 0x7f, 0x00, 0x7f, 0x00,
   0xff, 0x01, 0xff, 0x01, 0xff, 0x07, 0xff, 0x07, 0xff, 0x1f, 0xff, 0x1f,
   0xff, 0x7f, 0xff, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0x3f, 0xff, 0x3f,
   0xff, 0x0f, 0xff, 0x0f, 0xff, 0x03, 0xff, 0x03, 0xff, 0x00, 0xff, 0x00,
   0x3f, 0x00, 0x3f, 0x00, 0x0f, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x03, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
#else
extern	char play_bits[];
#endif
