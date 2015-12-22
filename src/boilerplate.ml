let gen_spawn ename =
  "void " ^ ename ^ "_spawn(){\n     " ^
    ename ^ "* data = malloc(sizeof(" ^ ename ^ "));
     entity_node* node = malloc(sizeof(entity_node));
     if(!data || !node) _seam_fatal(\"Allocation error!\");

     node->step = &" ^ ename ^ "_step;
     node->render = &" ^ ename ^ "_render;
     node->data = data;
     node->next = NULL;

     entity_node* curr = ehead;
     while(curr && curr->next) curr = next->next;

     if(curr)
       curr->next = node;
     else
       ehead = node;

     " ^ ename ^ "_start(data);
}"
