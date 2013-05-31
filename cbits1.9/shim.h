#include "/usr/lib/ruby/1.8/x86_64-linux/ruby.h"

struct s_dispatch {
	char * classname;
	char * methodname;
	int nbargs;
	VALUE args[16];
};

VALUE safeCall(VALUE args);

