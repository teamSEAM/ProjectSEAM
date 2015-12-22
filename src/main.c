#include "lib.h"
#include "gen.h" /* Generated program code */

#define TICK_DURATION 1000 / 60

int main(int argc, char** argv){
	//Start SDL
	if(SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_EVENTS) < 0){
		_seam_fatal("Failed to start SDL!");
	}

	//Exported from lib.h
	entity_node* entity_head = NULL;
	alloc_node* alloc_head = NULL;

	//Create the required World object that populates entity list
	__World_spawn(); //Guanteed to exist

	//Main program loop
	int running = 1;
	uint32_t ticks = SDL_GetTicks();
	while(running){
		//Call step functions
		entity_node* curr = ehead;
		if(!curr) running = 0;
		
		while(curr){
			curr->step(curr->data); 
			curr = curr->next;
		}

		//Call render functions
		curr = ehead;
		while(curr){
			curr->render(curr->data);
			curr = curr->next;
		}

		//Collect garbage
		_gc();
	
		//Update screen
		if(window_inited) SDL_UpdateWindowSurface(sdl_window);

		//Cap simulation at 30 FPS
		int time_diff = SDL_GetTicks() - ticks;
		int wait_duration = TICK_DURATION - time_diff;

		if(wait_duration < 0){
			fprintf(stderr, "Simulation slower than 30FPS by %d ms/frame\n",
				-wait_duration);
		} else {
			_screen_delay(wait_duration);
		}
		ticks = SDL_GetTicks();
	}

	//Exit SDL
	SDL_Quit();
    return 0;
}
