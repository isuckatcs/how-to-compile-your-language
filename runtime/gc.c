#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct Metadata {
  const int32_t offsetCnt;
  const int32_t offsets[];
};

struct AllocHeader {
  struct AllocHeader *next;
  const struct Metadata *metadata;
  int marked;
};

static struct AllocHeader *allocatedBlocks = NULL;

struct ShadowStackFrame {
  struct ShadowStackFrame *parent;
  const int32_t *rootCnt;
  void *roots[];
};

extern struct ShadowStackFrame *llvm_gc_root_chain;

void *gcAlloc(size_t size, const struct Metadata *metadata) {
  size_t offset = sizeof(struct AllocHeader);
  void *ptr = calloc(1, offset + size);

  struct AllocHeader *header = (struct AllocHeader *)ptr;
  header->next = allocatedBlocks;
  header->metadata = metadata;

  fprintf(stderr, "alloc:\n  addr: %p, data: %p (%ld byte), meta: %p\n", ptr,
          ptr + offset, size, metadata);
  if (metadata)
    for (int i = 0; i < metadata->offsetCnt; ++i)
      printf("  %d\n", metadata->offsets[0]);

  allocatedBlocks = header;
  return ptr + offset;
}

static void mark(void *root) {
  // FIXME: is it always null if not yet initialized?
  if (!root)
    return;

  struct AllocHeader *header = root - sizeof(struct AllocHeader);
  if (header->marked)
    return;

  header->marked = 1;

  fprintf(stderr, "  addr: %p, data: %p, value: %f\n", header, root,
          *(double *)root);

  const struct Metadata *metadata = header->metadata;
  if (!metadata)
    return;

  for (int i = 0; i < metadata->offsetCnt; ++i)
    mark(*(void **)(root + metadata->offsets[i]));
}

void gcMark() {
  fprintf(stderr, "mark (reachable):\n");

  struct ShadowStackFrame *currentFrame = llvm_gc_root_chain;
  while (currentFrame) {
    for (int32_t i = 0; i < *currentFrame->rootCnt; ++i)
      mark(currentFrame->roots[i]);

    currentFrame = currentFrame->parent;
  }
}

void gcSweep() {
  fprintf(stderr, "sweep (freed):\n");
  struct AllocHeader **blockPtrPtr = &allocatedBlocks;

  while (*blockPtrPtr) {
    struct AllocHeader *blockPtr = *blockPtrPtr;

    if (blockPtr->marked) {
      blockPtr->marked = 0;
      blockPtrPtr = &blockPtr->next;
      continue;
    }

    *blockPtrPtr = blockPtr->next;
    fprintf(stderr, "  addr: %p, data: %p, value: %f\n", blockPtr,
            (void *)blockPtr + sizeof(struct AllocHeader),
            *(double *)((void *)blockPtr + sizeof(struct AllocHeader)));
    free(blockPtr);
  }
}
