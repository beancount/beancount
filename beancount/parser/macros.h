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
