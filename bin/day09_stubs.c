#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <string.h>

int *int_array_set(int *offset, int num_blocks, int val) {
  for (int i = 0; i < num_blocks; i++) {
    offset[i] = val;
  }
  return offset + num_blocks;
}

int get_block_size(int id, const char *map) {
  if (id > 0) {
    id--;
    return *(map + (id * 2)) - '0';
  }
  return 0;
}

int get_available_space_after(int id, const char *map) {
  if (id > 0) {
    id--;
    return *(map + (id * 2) + 1) - '0';
  }
  return 0;
}

void defrag(int *disk, size_t size, const char *map, int hd, int tail) {
  if (hd >= tail) {
    return;
  }

  int id = disk[hd];
  if (id == 0 && disk[tail] > 0) {
    int tval = disk[tail];
    disk[tail] = 0;
    disk[hd] = tval;

    hd++;
    tail--;
    return defrag(disk, size, map, hd, tail);
  } else {
    if (id > 0) {
      hd += *(map + ((id - 1) * 2)) - '0';
    }

    if (disk[tail] == 0) {
      while (disk[tail] == 0) {
        tail--;
      }
    }
    return defrag(disk, size, map, hd, tail);
  }
}

void set_disk_data(const char *hd, const char *tail, int *disk) {
  int *offset = disk;

  int id = 1;

  while (hd != tail) {
    int num_blocks = *hd - '0';
    offset = int_array_set(offset, num_blocks, id);
    hd++;

    id++;

    int num_spaces = *hd - '0';
    offset = int_array_set(offset, num_spaces, 0);
    hd++;
  }
  ({
    // final block
    int num_blocks = *hd - '0';
    offset = int_array_set(offset, num_blocks, id);
  });
}

CAMLprim value defrag_disk(value _str) {
  CAMLparam1(_str);
  CAMLlocal1(result);
  const char *str = String_val(_str);

  const char *hd = str;
  const char *tail;
  size_t size = 0;
  int len_input = 0;
  while (*hd) {
    size += *hd - '0';
    tail = hd;
    len_input++;
    hd++;
  }

  int disk[size];
  set_disk_data(str, tail, disk);

  hd = str;
  defrag(disk, size, str, 0, size - 1);

  result = caml_alloc(size, 0);

  for (size_t i = 0; i < size; i++) {
    Store_field(result, i, Val_int(disk[i]));
  }
  CAMLreturn(result);
}
