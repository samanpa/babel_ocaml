#include <babel.h>
#include <stdio.h>

extern "C" BABEL_EXPORT int int_add (int x, int y) {
	return x + y;
}

extern "C" BABEL_EXPORT int int_minus (int x, int y) {
	return x - y;
}

extern "C" BABEL_EXPORT int int_div (int x, int y) {
	return x / y;
}

extern "C" BABEL_EXPORT int int_mul (int x, int y) {
	return x * y;
}

extern "C" BABEL_EXPORT double double_int_add (double x, int y) {
	printf ("%f %d\n", x, y);
	return x + y;
}

extern "C" BABEL_EXPORT bool int_gt (int x, int y) {
	return x > y;
}

extern "C" BABEL_EXPORT bool int_lt (int x, int y) {
	return x < y;
}
