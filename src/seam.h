typedef enum {
	STRUCT, STRING, INT, FLOAT, ENTITY
} variable_type;

typedef struct {
	void* value, //Value, gotten by casting
	enum variable_type type, //Type of property (set by compiler)
	
	int id, //Identifier assigned to the property by compiler
	char* name, //Name (used for debugging)
	
	prop* next
} prop;

typedef struct  {
	void* start_func = ..., //Could be = NULL, in which case it isn't called on step
	void* stop_func = ...,
	void* step_func = ...,
	void* render_func = ...,
	
	int id, //Identifier assigned to property by compiler
	char* name, //Name (used for debugging)
	
	prop* prop_head,
	int prop_count,
	
	entity* next //The next entity staged to be run
} entity;

typedef struct {
	/* Entities */
	entity* entity_head = NULL,
	int entity_count = 0,

	/* Global variables */ 
	prop* prop_head = NULL,
	int prop_count = 0
} environment;
