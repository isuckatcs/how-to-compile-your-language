#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t byte;

struct Metadata {
  const int32_t size;
  const int32_t offsetCnt;
  const int32_t offsets[];
};

enum Color {
  WHITE,
  GREY,
  BLACK,
};

struct AllocHeader {
  enum Color color;
  int32_t size;
  struct AllocHeader *next;
  const struct Metadata *metadata;
};

static struct AllocHeader *allocatedBlocks = NULL;
static size_t allocatedBlockCount = 0;

static byte **greyStack = NULL;
static size_t greyStackSize = 0;

struct FrameInfo {
  const int32_t rootCnt;
  const int32_t metaCnt;
  const struct Metadata *metas[];
};

struct ShadowStackFrame {
  struct ShadowStackFrame *parent;
  struct FrameInfo *frameInfo;
  byte *roots[];
};

struct ShadowStackFrame *llvm_gc_root_chain;

static const size_t minThreshold = 4 * sizeof(struct AllocHeader);
static size_t threshold = minThreshold;
static size_t heapSize = 0;

static int32_t gcCycle = 0;

enum Phase {
  ALLOC,
  MARK,
  SWEEP,
};

static void gcLog(enum Phase phase, struct AllocHeader *block) {
  if (!getenv("YL_GC_DUMP"))
    return;

  switch (phase) {
  case ALLOC:
    printf("[%d] alloc", gcCycle);
    break;
  case MARK:
    printf("[%d] mark", gcCycle);
    break;
  case SWEEP:
    printf("[%d] sweep", gcCycle);
    break;
  }

  if (block) {
    byte *data = (byte *)(block + 1);
    printf(" %p data: %p (%d B)", (void *)block, (void *)data, block->size);

    if (block->metadata) {
      printf(" offsets:");
      for (int i = 0; i < block->metadata->offsetCnt; ++i) {
        int32_t offset = block->metadata->offsets[i];
        printf(" {%d %p}", offset, *(void **)(data + offset));
      }
    }
  }

  printf(" {heap: %ld B, threshold: %ld B}\n", heapSize, threshold);
}

// Roots marked with `@llvm.gcroot()` are automatically intialized to `null`
// in the `gc-lowering` pass, and pointers inside allocated objects are also
// `null` initially because `calloc()` is used.
static void mark(byte *root) {
  if (!root)
    return;

  struct AllocHeader *header = (struct AllocHeader *)root - 1;
  if (header->color != WHITE)
    return;

  header->color = GREY;
  gcLog(MARK, header);

  greyStack[greyStackSize] = root;
  ++greyStackSize;
}

static void markChildren(byte *rootAddr, const struct Metadata *meta) {
  //
  //    v rootAddr   offsets = [8, 16]
  //     --------------------------
  //    | double | byte * | byte * |
  //     --------------------------
  //    0        8        16       24
  //
  if (!meta)
    return;

  for (int i = 0; i < meta->offsetCnt; ++i)
    mark(*(byte **)(rootAddr + meta->offsets[i]));
}

static void markRootNodes() {
  struct ShadowStackFrame *currentFrame = llvm_gc_root_chain;
  while (currentFrame) {
    const struct FrameInfo *frameInfo = currentFrame->frameInfo;
    int32_t i = 0;
    byte **rootPtr = currentFrame->roots;

    // These are composite roots with at least 1 field directly pointing to an
    // allocated block.
    //
    //    v rootPtr                v nextRootAddr
    //     -------------------------------------------
    //    | double | byte * | bool | byte * |   ...
    //     -------------------------------------------
    //    ^~~~~~~ meta->size ~~~~~~^
    //
    while (i < frameInfo->metaCnt) {
      byte *rootAddr = (byte *)rootPtr;
      const struct Metadata *meta = frameInfo->metas[i];

      markChildren(rootAddr, meta);

      byte *nextRootAddr = rootAddr + meta->size;
      rootPtr = (byte **)(nextRootAddr);
      ++i;
    }

    // These roots are direct pointers to allocated blocks.
    //
    //    ---------------------------
    //       ...   | byte * | byte * |
    //    ---------------------------
    //             ^ rootPtr
    //
    while (i < frameInfo->rootCnt) {
      mark(*rootPtr);
      ++rootPtr;
      ++i;
    }

    currentFrame = currentFrame->parent;
  }
}

void gcMark() {
  size_t requiredStackSlots = allocatedBlockCount;

  struct ShadowStackFrame *frame = llvm_gc_root_chain;
  while (frame) {
    requiredStackSlots += frame->frameInfo->rootCnt;
    frame = frame->parent;
  }

  greyStack = malloc(requiredStackSlots * sizeof(byte *));

  markRootNodes();

  while (greyStackSize != 0) {
    byte *root = greyStack[--greyStackSize];
    struct AllocHeader *header = (struct AllocHeader *)root - 1;

    markChildren(root, header->metadata);
    header->color = BLACK;
  }

  free(greyStack);
  greyStack = NULL;
}

void gcSweep() {
  struct AllocHeader **blockPtrPtr = &allocatedBlocks;

  while (*blockPtrPtr) {
    struct AllocHeader *blockPtr = *blockPtrPtr;

    if (blockPtr->color == BLACK) {
      blockPtr->color = WHITE;
      blockPtrPtr = &blockPtr->next;
      continue;
    }

    --allocatedBlockCount;
    heapSize -= blockPtr->size + sizeof(struct AllocHeader);
    *blockPtrPtr = blockPtr->next;

    gcLog(SWEEP, blockPtr);
    free(blockPtr);
  }

  threshold = heapSize * 2;
  if (threshold < minThreshold)
    threshold = minThreshold;

  gcLog(SWEEP, NULL);
  ++gcCycle;
}

void *gcAlloc(int32_t size, const struct Metadata *metadata) {
  size_t blockSize = sizeof(struct AllocHeader) + size;

  struct AllocHeader *block = calloc(1, blockSize);
  block->metadata = metadata;
  block->size = size;
  block->next = allocatedBlocks;

  ++allocatedBlockCount;
  heapSize += blockSize;
  allocatedBlocks = block;
  gcLog(ALLOC, block);

  if (heapSize > threshold) {
    block->color = BLACK;
    gcLog(MARK, block);

    gcMark();
    gcSweep();
  }

  return block + 1;
}
