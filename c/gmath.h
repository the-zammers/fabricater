#ifndef GMATH_H
#define GMATH_H

#include "matrix.h"
#include "ml6.h"
#include "symtab.h"

// constants for lighting
#define LOCATION 0
#define COLOR 1
#define RED 0
#define GREEN 1
#define BLUE 2


#define SPECULAR_EXP 4

// lighting functions
color get_lighting( double *normal, double *view, color alight, struct light *lights, struct constants *reflect);
double * calculate_ambient(color alight, struct constants *reflect );
double * calculate_diffuse(struct light *lights, struct constants *reflect, double *normal );
double *  calculate_specular(struct light *lights, struct constants *reflect, double *view, double *normal );
color make_color( double c[3] );

// vector functions
void normalize( double *vector );
double dot_product( double *a, double *b );
double *calculate_normal(struct matrix *polygons, int i);

#endif
