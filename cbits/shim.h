#include <ruby.h>
#include <ruby/intern.h>

void ruby_initialization(void);

struct s_dispatch {
	VALUE receiver;
	ID methodid;
	int nbargs;
	VALUE args[16];
};

VALUE safeCall(VALUE args);
long arrayLength(VALUE r);

VALUE newFloat(double d);
int rubyType(VALUE obj);
VALUE int2num(long x);
long num2long(VALUE v);
double num2dbl(VALUE v);
VALUE id2sym(ID i);
ID sym2id(VALUE v);
