
#ifndef DISPLAY_H
#define DISPLAY_H

#include "ml6.h"
#define SDL_IMG_NAME "image.png"


void plot(screen s, zbuffer zb, color c, int x, int y, double z);
void clear_screen( screen s);
void clear_zbuffer( zbuffer zb );
void save_ppm( screen s, char *file);
void save_ppm_ascii( screen s, char *file);
void save_extension( screen s, char *file);
void display( screen s);
void sdl_display(const char* filename);

#endif
