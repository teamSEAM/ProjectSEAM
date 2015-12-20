#include <stdio.h>
#include "lib.h"

#include <SDL2/SDL.h>

void _seam_print(char* message)
{
    fprintf(stdout, "%s\n", message);
}


void start_SDL(int width, int height)
{
    if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
	{
		printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
	}
	else
	{
        // If SDL loaded, create a window and a surface we can draw to
		sdlWindow = SDL_CreateWindow( "TEAM SEAM", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
            width, height, SDL_WINDOW_SHOWN );
		if( sdlWindow == NULL )
		{
			printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
		}
		else
		{
			sdlScreenSurface = SDL_GetWindowSurface( sdlWindow );
		}
	}

}

void stop_SDL()
{
    SDL_FreeSurface( sdlScreenSurface);
	SDL_DestroyWindow( sdlWindow );
	SDL_Quit();

}

void delay(int milliseconds)
{
    SDL_Delay( milliseconds );
}
