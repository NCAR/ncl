
#ifndef __rgba2png__
#define __rgba2png__

int rgba2png(unsigned int * Red,
        unsigned int * Green,
        unsigned int * Blue,
        unsigned int * Alpha,
        size_t width,
        size_t height,
        int nFrames,
        char * BaseFileName);

#endif  