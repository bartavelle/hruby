#include "shim.h"

VALUE safeCall(VALUE args)
{
	struct s_dispatch * d = (struct s_dispatch *) args;
	VALUE myclass = rb_const_get(rb_cObject, rb_intern(d->classname));
	VALUE o = rb_funcall2(myclass, rb_intern(d->methodname), d->nbargs, d->args);
	free(d->methodname);
	free(d->classname);
	return o;
}

long arrayLength(VALUE r)
{
	return RARRAY_LEN(r);
}

VALUE newFloat(double d) {
#ifdef RUBY2
	rb_float_new_in_heap(d);
#else
	rb_float_new(d);
#endif
}

int rubyType(VALUE obj) {
	return rb_type(obj);
}

VALUE int2num(long x) {
	return INT2NUM(x);
}

long num2long(VALUE v) {
	return NUM2LONG(v);
}

double num2dbl(VALUE v) {
	NUM2DBL(v);
}

