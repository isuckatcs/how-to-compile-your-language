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
  int32_t size;
  int32_t marked;
};

static struct AllocHeader *allocatedBlocks = NULL;

struct FrameInfo {
  const int32_t rootCnt;
  const int32_t metaCnt;
  const struct Metadata *metas[];
};

struct ShadowStackFrame {
  struct ShadowStackFrame *parent;
  struct FrameInfo *frameInfo;
  void *roots[];
};

struct ShadowStackFrame *llvm_gc_root_chain;

static const int32_t minThreshold = 16;
static int32_t threshold = minThreshold;
static int32_t heapSize = 0;

static int32_t markCycle = 0;
static int32_t sweepCycle = 0;

enum Phase {
  ALLOC,
  MARK,
  SWEEP,
};

static void log(enum Phase phase, struct AllocHeader *block) {
  if (!getenv("YL_GC_DUMP"))
    return;

  void *data = (void *)block + sizeof(struct AllocHeader);
  switch (phase) {
  case ALLOC:
    printf("alloc");
    break;
  case MARK:
    printf("[%d] mark", markCycle);
    break;
  case SWEEP:
    printf("[%d] sweep", sweepCycle);
    break;
  }

  if (block) {
    printf(" @%p, data: %p (%d B)", block, data, block->size);

    if (block->metadata) {
      printf(" offsets:");
      for (int i = 0; i < block->metadata->offsetCnt; ++i) {
        int32_t offset = block->metadata->offsets[i];
        printf(" {%d @%p}", offset, *(void **)(data + offset));
      }
    }
  }

  printf(" {heap: %d B, threshold: %d B} \n", heapSize, threshold);
}

static void mark(void *root);

static void markChildren(void *root, const struct Metadata *metadata) {
  if (!root || !metadata)
    return;

  for (int i = 0; i < metadata->offsetCnt; ++i)
    mark(*(void **)(root + metadata->offsets[i]));
}

// Roots marked with `@llvm.gcroot()` are automatically intialized to `null`
// in the `gc-lowering` pass, and pointers inside allocated objects are also
// `null` initially because `calloc()` is used.
static void mark(void *root) {
  if (!root)
    return;

  struct AllocHeader *header = root - sizeof(struct AllocHeader);
  if (header->marked)
    return;

  header->marked = 1;
  log(MARK, header);

  markChildren(root, header->metadata);
}

void gcMark() {
  struct ShadowStackFrame *currentFrame = llvm_gc_root_chain;
  while (currentFrame) {
    int32_t i = 0;
    const struct FrameInfo *frameInfo = currentFrame->frameInfo;

    while (i < frameInfo->metaCnt) {
      markChildren(&currentFrame->roots[i], frameInfo->metas[i]);
      ++i;
    }

    while (i < frameInfo->rootCnt) {
      mark(currentFrame->roots[i]);
      ++i;
    }

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

    heapSize -= blockPtr->size;
    *blockPtrPtr = blockPtr->next;

    log(SWEEP, blockPtr);
    free(blockPtr);
  }

  threshold = heapSize * 2;
  if (threshold < minThreshold)
    threshold = minThreshold;

  log(SWEEP, NULL);
  ++sweepCycle;
}

void *gcAlloc(int32_t size, const struct Metadata *metadata) {
  size_t offset = sizeof(struct AllocHeader);
  void *ptr = calloc(1, offset + size);

  struct AllocHeader *header = (struct AllocHeader *)ptr;
  header->metadata = metadata;
  header->size = size;
  header->next = allocatedBlocks;

  heapSize += size;
  allocatedBlocks = header;
  log(ALLOC, header);

  if (heapSize > threshold) {
    mark(ptr + offset);
    gcMark();
    gcSweep();
  }

  return ptr + offset;
}
