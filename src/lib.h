#ifndef __LIB_H__
#define __LIB_H__
typedef struct {
    void* start;
    void* stop;
    void* step;
    void* render;

    void* data;
} entity;

void _seam_print(char* message);

#endif
