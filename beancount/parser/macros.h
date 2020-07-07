#ifndef BEANCOUNT_PARSER_MACROS_H
#define BEANCOUNT_PARSER_MACROS_H

#define _CC_FUNC_01(FN, X, ...) FN(X)
#define _CC_FUNC_02(FN, X, ...) FN(X); _CC_FUNC_01(FN, __VA_ARGS__)
#define _CC_FUNC_03(FN, X, ...) FN(X); _CC_FUNC_02(FN, __VA_ARGS__)
#define _CC_FUNC_04(FN, X, ...) FN(X); _CC_FUNC_03(FN, __VA_ARGS__)
#define _CC_FUNC_05(FN, X, ...) FN(X); _CC_FUNC_04(FN, __VA_ARGS__)
#define _CC_FUNC_06(FN, X, ...) FN(X); _CC_FUNC_05(FN, __VA_ARGS__)
#define _CC_FUNC_07(FN, X, ...) FN(X); _CC_FUNC_06(FN, __VA_ARGS__)
#define _CC_FUNC_08(FN, X, ...) FN(X); _CC_FUNC_07(FN, __VA_ARGS__)
#define _CC_FUNC_09(FN, X, ...) FN(X); _CC_FUNC_08(FN, __VA_ARGS__)
#define _CC_FUNC_10(FN, X, ...) FN(X); _CC_FUNC_09(FN, __VA_ARGS__)

#define _CC_FUNC_SEQ(_01, _02, _03, _04, _05, _06, _07, _08, _09, _10, NAME, ...) NAME

/**
 * A macro that applies the function @FN to all successive arguments.
 */
#define _CC_FUNC(FN, ...)                               \
        _CC_FUNC_SEQ(__VA_ARGS__,                       \
                     _CC_FUNC_10,                       \
                     _CC_FUNC_09,                       \
                     _CC_FUNC_08,                       \
                     _CC_FUNC_07,                       \
                     _CC_FUNC_06,                       \
                     _CC_FUNC_05,                       \
                     _CC_FUNC_04,                       \
                     _CC_FUNC_03,                       \
                     _CC_FUNC_02,                       \
                     _CC_FUNC_01) (FN, __VA_ARGS__)

/* An example is the best way to exaplain how the above preprocessor
 * magic works. Assuume a macro call like the following:
 *
 *   _CC_FUNC(putc, "a", "b", "c")
 *
 * This gets expanded to:
 *
 *   _CC_FUNC_SEQ("a", "b", "c", _CC_FUNC_10, ... _CC_FUNC_03, _CC_FUNC_02, _CC_FUNC_01) (putc, "a", "b", "c")
 *
 * thus the only purpose of the first occurrence of __VA_ARGS__ in the
 * definition of _CC_FUNC is to shift the arguments to _CC_FUNC_SEQ to
 * the right. In turn _CC_FUNC_SEQ always picks its 11th argument as
 * its expansion and discards all others. Because of how the arguments
 * to _CC_FUNC_SEQ are sorted this is the right _CC_FUNC_xx iterative
 * macro definition to exapns to the right number of arguments. In
 * this example the exapnsion results in:
 *
 *    _CC_FUNC_03(putc, "a", "b", "c")
 *
 * Which iteratively expands to:
 *
 *    putc("a"); _CC_FUNC_02(putc, "b", "c")
 *    putc("a"); putc("b"); _CC_FUNC_01(putc, "c");
 *    putc("a"); putc("b"); putc("c");
 *
 * If the _CC_FUNC() macro needs to be called with more than 10
 * arguments, these macro definitions need to be extended using the
 * same patterns.
 */

#endif /* BEANCOUNT_PARSER_MACROS_H */
