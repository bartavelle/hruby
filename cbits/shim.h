#include <ruby.h>

struct s_dispatch {
	char * classname;
	char * methodname;
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
