let stubs_ctor = ["start"; "stop"]
let stubs_action = ["step"; "render"]
let stubs_helper = ["spawn"; "destroy"]

let gen_spawn ename =
  ename ^ "* " ^ ename ^ "_spawn(){\n     " ^
    ename ^ " *data = malloc(sizeof(" ^ ename ^ "));
    entity_node *node = malloc(sizeof(entity_node));
    if(!data || !node) _seam_fatal(\"Allocation error!\");

    node->step = &" ^ ename ^ "_step;
    node->render = &" ^ ename ^ "_render;
    node->data = data;
    node->next = NULL;

    entity_node *curr = ehead;
    while(curr && curr->next) curr = curr->next;

    if(curr)
        curr->next = node;
    else
        ehead = node;

    " ^ ename ^ "_start(data);
    return data;
}"

let gen_destroy ename =
  "void " ^ ename ^ "_destroy(" ^ ename ^ " *this){\n     " ^
    ename ^ "_stop(this);

    entity_node *curr = ehead;
    entity_node *prev = NULL;
    while(curr) {
        if(curr->data == this) break;
        prev = curr;
        curr = curr->next;
    }

    if(prev)
        prev->next = curr->next;
    else
    ehead = curr->next;

    free(this);
    free(curr);
}"
