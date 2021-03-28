const
  TAB = #9;

const

  bit_val: array[0..31] of iSize32 = (
    %00000000000000000000000000000001, %00000000000000000000000000000010,
    %00000000000000000000000000000100, %00000000000000000000000000001000,
    %00000000000000000000000000010000, %00000000000000000000000000100000,
    %00000000000000000000000001000000, %00000000000000000000000010000000,
    %00000000000000000000000100000000, %00000000000000000000001000000000,
    %00000000000000000000010000000000, %00000000000000000000100000000000,
    %00000000000000000001000000000000, %00000000000000000010000000000000,
    %00000000000000000100000000000000, %00000000000000001000000000000000,
    %00000000000000010000000000000000, %00000000000000100000000000000000,
    %00000000000001000000000000000000, %00000000000010000000000000000000,
    %00000000000100000000000000000000, %00000000001000000000000000000000,
    %00000000010000000000000000000000, %00000000100000000000000000000000,
    %00000001000000000000000000000000, %00000010000000000000000000000000,
    %00000100000000000000000000000000, %00001000000000000000000000000000,
    %00010000000000000000000000000000, %00100000000000000000000000000000,
    %01000000000000000000000000000000, %10000000000000000000000000000000
    );

  bit_mask: array[0..31] of iSize32 = (
    %11111111111111111111111111111110, %11111111111111111111111111111101,
    %11111111111111111111111111111011, %11111111111111111111111111110111,
    %11111111111111111111111111101111, %11111111111111111111111111011111,
    %11111111111111111111111110111111, %11111111111111111111111101111111,
    %11111111111111111111111011111111, %11111111111111111111110111111111,
    %11111111111111111111101111111111, %11111111111111111111011111111111,
    %11111111111111111110111111111111, %11111111111111111101111111111111,
    %11111111111111111011111111111111, %11111111111111110111111111111111,
    %11111111111111101111111111111111, %11111111111111011111111111111111,
    %11111111111110111111111111111111, %11111111111101111111111111111111,
    %11111111111011111111111111111111, %11111111110111111111111111111111,
    %11111111101111111111111111111111, %11111111011111111111111111111111,
    %11111110111111111111111111111111, %11111101111111111111111111111111,
    %11111011111111111111111111111111, %11110111111111111111111111111111,
    %11101111111111111111111111111111, %11011111111111111111111111111111,
    %10111111111111111111111111111111, %01111111111111111111111111111111
    );

  count_of_bits: array[0..255] of iSize8 = (
    0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
    );

  parity_bit: array[0..255] of boolean = (
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True
    );

  bool_bit: array[boolean] of iSize8 = (0, 1);
  bool_bit8: array[boolean] of iSize8 = (0, $80);
  negbool_bit: array[boolean] of iSize8 = (1, 0);

  endian_offset: array[boolean, 0..1] of iSize8 = ((1, 0), (0, 1));


