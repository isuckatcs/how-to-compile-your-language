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

enum Phase {
  ALLOC,
  MARK,
  SWEEP,
};

static int markCycle = 0;
static int sweepCycle = 0;

static void log(enum Phase phase, struct AllocHeader *block, size_t size) {
  if (!getenv("YL_GC_DUMP"))
    return;

  void *data = (void *)block + sizeof(struct AllocHeader);
  switch (phase) {
  case ALLOC:
    fprintf(stderr, "alloc %ld @%p, data: %p", size, block, data);
    break;
  case MARK:
    fprintf(stderr, "[%d] mark @%p, data: %p", markCycle, block, data);
    break;
  case SWEEP:
    fprintf(stderr, "[%d] sweep @%p, data: %p", sweepCycle, block, data);
    break;
  }

  if (block->metadata) {
    fprintf(stderr, " offsets:");
    for (int i = 0; i < block->metadata->offsetCnt; ++i) {
      int32_t offset = block->metadata->offsets[i];
      fprintf(stderr, " {%d @%p}", offset, *(void **)(data + offset));
    }
  }

  fprintf(stderr, "\n");
}

extern struct ShadowStackFrame *llvm_gc_root_chain;

void *gcAlloc(size_t size, const struct Metadata *metadata) {
  size_t offset = sizeof(struct AllocHeader);
  void *ptr = calloc(1, offset + size);

  struct AllocHeader *header = (struct AllocHeader *)ptr;
  header->next = allocatedBlocks;
  header->metadata = metadata;

  log(ALLOC, header, size);

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
  log(MARK, header, 0);

  const struct Metadata *metadata = header->metadata;
  if (!metadata)
    return;

  for (int i = 0; i < metadata->offsetCnt; ++i)
    mark(*(void **)(root + metadata->offsets[i]));
}

void gcMark() {
  struct ShadowStackFrame *currentFrame = llvm_gc_root_chain;
  while (currentFrame) {
    for (int32_t i = 0; i < *currentFrame->rootCnt; ++i)
      mark(currentFrame->roots[i]);

    currentFrame = currentFrame->parent;
  }

  ++markCycle;
}

void gcSweep() {
  struct AllocHeader **blockPtrPtr = &allocatedBlocks;

  while (*blockPtrPtr) {
    struct AllocHeader *blockPtr = *blockPtrPtr;

    if (blockPtr->marked) {
      blockPtr->marked = 0;
      blockPtrPtr = &blockPtr->next;
      continue;
    }

    log(SWEEP, blockPtr, 0);

    *blockPtrPtr = blockPtr->next;
    free(blockPtr);
  }

  ++sweepCycle;
}
