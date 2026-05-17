#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct AllocHeader {
  struct AllocHeader *next;
  int marked;
};

static struct AllocHeader *allocatedBlocks = NULL;

struct ShadowStackFrame {
  struct ShadowStackFrame *parent;
  const int32_t *rootCnt;
  void *roots[];
};

extern struct ShadowStackFrame *llvm_gc_root_chain;

void *gcAlloc(size_t size) {
  size_t offset = sizeof(struct AllocHeader);
  void *ptr = calloc(1, offset + size);

  struct AllocHeader *header = (struct AllocHeader *)ptr;
  header->next = allocatedBlocks;
  allocatedBlocks = header;

  printf("alloc: %p, data: %p (%ld byte)\n", ptr, ptr + offset, size);

  return ptr + offset;
}

void gcMark() {
  printf("mark\n");

  struct ShadowStackFrame *currentFrame = llvm_gc_root_chain;
  while (currentFrame) {
    int32_t rootCnt = *currentFrame->rootCnt;
    for (int32_t i = 0; i < rootCnt; ++i) {
      void *root = currentFrame->roots[i];
      struct AllocHeader *header = root - sizeof(struct AllocHeader);

      printf("alloc: %p, data: %p, value: %f\n", header, root, *(double *)root);

      header->marked = 1;
    }

    currentFrame = currentFrame->parent;
  }
}

void gcSweep() { printf("sweep\n"); }
