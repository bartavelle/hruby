#include "shim.h"
#ifdef RUBY2
#include <ruby/encoding.h>
#endif

void ruby_initialization(void) {
	RUBY_INIT_STACK;
	ruby_init();
	ruby_init_loadpath();
#ifdef RUBY2
	VALUE encoding = rb_enc_from_encoding(rb_filesystem_encoding());
	rb_enc_set_default_internal(encoding);
	rb_enc_set_default_external(encoding);
	void rb_encdb_declare(const char *name);
	rb_encdb_declare("ASCII-8BIT");
	rb_encdb_declare("US-ASCII");
	rb_encdb_declare("UTF-8");
#endif
}

VALUE id2sym(ID i) {
	return ID2SYM(i);
}

ID sym2id(VALUE v) {
	return SYM2ID(v);
}

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

