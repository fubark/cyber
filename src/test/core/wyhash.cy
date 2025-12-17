use test

type TestVector:
    exp   int
    seed  int
    input str

-- Run https://github.com/wangyi-fudan/wyhash/blob/77e50f267fbc7b8e2d09f2d455219adb70ad4749/test_vector.cpp directly.
-- With: -DWYHASH_32BIT_MUM
vectors := [_]TestVector{
    { seed = 0, exp = 0x4b80acaa567a5c84, input = '' },
    { seed = 1, exp = 0xb78e7daf065068fa, input = 'a' },
    { seed = 2, exp = 0xd176a04d1bbbff00, input = 'abc' },
    { seed = 3, exp = 0x8e97d7095d8ad10f, input = 'message digest' },
    { seed = 4, exp = 0x857e6e009e6f847c, input = 'abcdefghijklmnopqrstuvwxyz' },
    { seed = 5, exp = 0x2d10fde4d0d281cf, input = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789' },
    { seed = 6, exp = 0x01aeafe0bc395545, input = '12345678901234567890123456789012345678901234567890123456789012345678901234567890' },
}

for vectors |i, t|:
    test.eq(t.exp, Wyhash.hash(t.seed, t.input.span()))

--cytest: pass