//Scratch work for C bits that will eventually be implemented in compiler
void _seam_fatal(char* err){
	fprintf(stderr, "%s\n", err);
	exit(1);
}

//Allocation list (primitive GC)
typedef struct {
	void* ptr;
	alloc_node* next;
	int ref_count;
} alloc_node;

alloc_node head = NULL;

void _make_node(void* ptr){
	alloc_node* curr = head;
	while(curr->next) { curr = curr->next; }
	
	//curr contains the last element
	alloc_node* new_node = malloc(sizeof(alloc_node));
	if(!new_node) _seam_fatal("Failed to allocate allocation node!");
	
	new_node->next = NULL;
	new_node->ptr = ptr;
	new_node->ref_count = 1; 
}

alloc_node* _find_alloc_node(void* ptr){
	alloc_node* curr = head;
	while(curr){
		if(curr == ptr) return curr;
	}

	_seam_fatal("Missing allocation node for pointer!");
	return NULL;
}

void _increment_refs(void* ptr){
	alloc_node* curr = _find_alloc_node(ptr);
	curr->ref_count = curr->ref_count + 1;
}

void _decrement_refs(void* ptr){
	alloc_node* curr = _find_alloc_node(ptr);
	curr->ref_count = curr->ref_count - 1;
}

void _gc(){
	alloc_node* curr = head;

	//Free up list elements
	while(curr != NULL){
		if(curr->ref_count == 0){
			free(curr->ptr);
		}

		curr = curr->next;
	}

	//Free nodes and invalidate pointer
	prev = NULL;
	curr = head;
	while(curr != NULL){
		alloc_node* next = curr->next;

		if(curr->ref_count == 0){
			if(prev != NULL) prev->next = NULL; //Invalidate last pointer if not head
			if(curr == head) head = next; //Move head, if needed

			curr = NULL;
			free(curr);
		}

		if(curr != NULL) prev = curr; //Move previous if this was valid
		curr = next;
	}
}
