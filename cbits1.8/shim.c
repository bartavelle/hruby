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

