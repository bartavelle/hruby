#include <ruby.h>
#ifdef RUBY21
#include <ruby/intern.h>
#endif

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

#ifdef RUBY18
#define RUBY_T_NONE   T_NONE
#define RUBY_T_OBJECT T_OBJECT
#define RUBY_T_CLASS  T_CLASS
#define RUBY_T_MODULE T_MODULE
#define RUBY_T_FLOAT  T_FLOAT
#define RUBY_T_STRING T_STRING
#define RUBY_T_REGEXP T_REGEXP
#define RUBY_T_ARRAY  T_ARRAY
#define RUBY_T_HASH   T_HASH
#define RUBY_T_STRUCT T_STRUCT
#define RUBY_T_BIGNUM T_BIGNUM
#define RUBY_T_FILE   T_FILE
#define RUBY_T_DATA   T_DATA
#define RUBY_T_MATCH  T_MATCH
#define RUBY_T_NIL    T_NIL
#define RUBY_T_TRUE   T_TRUE
#define RUBY_T_FALSE  T_FALSE
#define RUBY_T_SYMBOL T_SYMBOL
#define RUBY_T_FIXNUM T_FIXNUM
#define RUBY_T_UNDEF  T_UNDEF
#define RUBY_T_NODE   T_NODE
#define RUBY_T_ICLASS T_ICLASS
#endif
