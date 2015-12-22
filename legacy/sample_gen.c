#include "lib.h"
#include "gen.h"

/* A sample gen.c. */
void program_ep(){
	_screen_out("--- TEST GC ---");
	
	char* buf = malloc(sizeof(char) * 100);
	char* buf2 = malloc(sizeof(char) * 100);
	char* buf3 = malloc(sizeof(char) * 100);

	sprintf(buf, "Buffer 1...");
	sprintf(buf2, "Buffer 2...");
	sprintf(buf3, "Buffer 3...");

	_screen_out(buf);

	_make_node(buf);
	_make_node(buf2);
	_make_node(buf3);
	_increment_refs(buf);
	_increment_refs(buf2);
	_increment_refs(buf3);

	_decrement_refs(buf);
	_gc();

	_screen_out("Buf freed...");

	_decrement_refs(buf3);
	_gc();

	_screen_out("Buf3 freed...");

	_decrement_refs(buf2);
	_gc();

	_screen_out("Buf2 freed...");

	_screen_out("All garbage collected.");

	_screen_out("--- TEST CONVERSIONS ---");
	
	char* sum = _string_join("Hello ", "world!");
	_screen_out(sum);

	char* int_str = _convert_int_to_str(500);
	_screen_out(int_str);

	char* float_str = _convert_float_to_str(50.897);
	_screen_out(float_str);
	
	_gc(); 
	
	_screen_out("--- TEST SCREEN/KEYBOARD BUILT-IN ---");
	_screen_init(640, 640);
	_screen_set_background(0xff00ffff);
	_screen_delay(1000);

	if(_keyboard_keydown(SDLK_LEFT)){
		_screen_out("Left is down!");
	} else {
		_screen_out("Left is up!");
	}

	texture* tex = _load_tex("kitty.bmp");
	_screen_draw(tex, 0, 0);	
	_unload_tex(tex);

	_screen_delay(2000);

	_screen_stop();
}
