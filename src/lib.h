#pragma once

#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct entity_node;
typedef struct entity_node {
	//Pointers kept to step and render functions so they can be called
	//by main
	void (*step)(void* in); //'in' cast to appropriate type from within
	void (*render)(void* in);

	void* data; 
	struct entity_node* next;
} entity_node;

struct alloc_node;
typedef struct alloc_node {
	void* ptr;
	struct alloc_node* next;
	int ref_count;
} alloc_node;

struct texture;
typedef struct texture {
	SDL_Surface* surface;
	int width;
	int height;
} texture;

struct keyboard_node;
typedef struct keyboard_node {
	int key;
	int press_type;

	struct keyboard_node* next;
} keyboard_node;

/* Core globals shared across all modules */
static entity_node* ehead = NULL; //Entity list
static alloc_node* ahead = NULL; //Allocation list
static int quit_requested = 0;

static SDL_Window* sdl_window = NULL; //SDL window
static SDL_Surface* sdl_screen_surface = NULL;
static int window_inited = 0; //Have we created an SDL window?
static keyboard_node* khead = NULL;

/* Memory management */
void _make_node(void* ptr);
alloc_node* _find_alloc_node(void* ptr);
void _increment_refs(void* ptr);
void _decrement_refs(void* ptr);
void _gc();

/* Runtime errors */
void _seam_fatal(char* err);

/* Entity mangement */

/* 'convert' entity */

char* _string_join(char* str1, char* str2);
char* _convert_int_to_str(int input);
char* _convert_float_to_str(float input);
float _convert_int_to_float(int input);
int _convert_float_to_int(float input);

/* 'screen' entity */

void _screen_out(char* message);
void _screen_init(int width, int height);
void _screen_stop();
void _screen_delay(int milliseconds); /* Not provided to user */
void _screen_set_background(int color);
void _screen_draw(texture* tex, int x, int y);

/* Texture loading */

texture* _load_tex(char* path);
void _unload_tex(texture* surface);

/* 'keyboard' entity */

void _keyboard_poll_events();
int _keyboard_event_check(int type, int code);
int _keyboard_keydown(int code);
int _keyboard_keyup(int code);
void _check_quit_requested();
