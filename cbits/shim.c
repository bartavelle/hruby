#include "shim.h"
#include <ruby/encoding.h>

void ruby_initialization(void) {
	RUBY_INIT_STACK;
	ruby_init();
	ruby_init_loadpath();
	VALUE encoding = rb_enc_from_encoding(rb_filesystem_encoding());
	rb_enc_set_default_internal(encoding);
	rb_enc_set_default_external(encoding);
	void rb_encdb_declare(const char *name);
	int rb_encdb_alias(const char *alias, const char *orig);
	rb_encdb_declare("ASCII-8BIT");
	rb_encdb_declare("US-ASCII");
	rb_encdb_declare("UTF-8");
	rb_encdb_alias("BINARY", "ASCII-8BIT");
	rb_encdb_alias("ASCII", "US-ASCII");
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
	return rb_funcall2(d->receiver, d->methodid, d->nbargs, d->args);
}

VALUE getRubyCObject(VALUE name)
{
	return rb_const_get(rb_cObject, rb_intern((const char*) name));
}

long arrayLength(VALUE r)
{
	return RARRAY_LEN(r);
}

VALUE newFloat(double d) {
	rb_float_new_in_heap(d);
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
