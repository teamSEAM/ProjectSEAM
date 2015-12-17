#ifndef __LIB_H__
#define __LIB_H__

#include <SDL2/SDL.h>


typedef struct {
    void* start;
    void* stop;
    void* step;
    void* render;

    void* data;
} entity;


static SDL_Window* sdlWindow = NULL;
static SDL_Surface* sdlScreenSurface = NULL;

void start_SDL(int width, int height);
void stop_SDL();
void delay(int milliseconds);



void _seam_print(char* message);

#endif
