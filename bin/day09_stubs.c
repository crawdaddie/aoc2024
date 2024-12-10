#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <string.h>

void print_disk(int *disk, size_t size) {
  for (int i = 0; i < size; i++) {
    if (disk[i] == 0) {
      printf(".");
    } else {
      printf("%d", disk[i] - 1);
    }
  }
  printf("\n");
}
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

  int padding = get_available_space_after(disk[hd], map);
  hd += get_block_size(disk[hd], map);

  while (hd <= tail) {
    print_disk(disk, size);

    hd++;
    tail--;
  }
}

void set_disk_data(const char *hd, const char *tail, const int *disk) {
  const int *offset = disk;

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

CAMLprim value process_string(value _str) {
  CAMLparam1(_str);
  // Get C string from OCaml string
  const char *str = String_val(_str);

  const char *hd = str;
  const char *tail;
  size_t size = 0;
  while (*hd) {
    size += *hd - '0';
    printf("iter: `%c`\n", *hd);
    tail = hd;
    hd++;
  }

  int disk[size];
  set_disk_data(str, tail, disk);
  print_disk(disk, size);

  hd = str;
  defrag(disk, size, str);
}
