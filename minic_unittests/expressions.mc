int256[3] array;
int256#[bool] hashmap;
struct bar {
  bool one;
  int256#[int8] two;
} mystruct;

void foo(int256 tmp) {
  tmp = tmp;
  tmp = 0;
	// Pointer deref not supported by backend yet
  // tmp = *tmp;

  tmp = !tmp;
  tmp = ~tmp;
  tmp = -tmp;

  tmp = 1 + 2 - 3 / 4 % 5 ** 6 & 7 | 8 ^ 9 << 10 >> 11 == 12;

	tmp = sha1 tmp;

  tmp = array[0];
  array[0] = tmp;

  tmp = hashmap[0];
  hashmap[0] = tmp;

  tmp = address;
  tmp = origin;
  tmp = caller;
  tmp = callvalue;
  tmp = coinbase;
  tmp = timestamp;
  tmp = number;

  tmp = balance(0);
  tmp = blockhash(0);

  tmp = mystruct.one;
  tmp = mystruct.two[0];

}
