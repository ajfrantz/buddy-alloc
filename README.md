# Practice "Buddy" Memory Allocator

This repository contains a practice implementation of a [buddy memory
allocator](https://en.wikipedia.org/wiki/Buddy_memory_allocation).  It's not
meant for production, and is basically untested.  I simply wanted to play with
how such a concept could be implemented.

The code is pretty low-level compared to the style I'd normally program in; a
lot of raw pointers and implicitly using memory as either allocated blocks or
lists.  This is meant to minimize overhead, but I haven't actually tried very
hard to optimize anything, so maybe that was overkill.

The main mechanism is to use:
 * A binary tree representing the used/free status of each block, stored in [an
   array representation](https://en.wikipedia.org/wiki/Binary_tree#Arrays), and
   storing only one bit per block.
    * The key insight here is that while there are four states (neither block
      used, only left used, only right used, both used), the _interesting_
      transitions occur only for (unused, allocating) -> split block, or
      (single node used, freeing) -> coalesce block.
    * That means we can get away with only tracking "split" blocks (only-left
      or only-right) and the context of the operation, collapsing the four
      states into two, allowing a single bit per node.
 * A circular linked list of "free" blocks, stored in an array, one for each
   "order" of block size.
    * The free list array allows us to quickly find the available blocks within
      each order, and the circular nature of the list both makes the list
      insert/remove code cleaner, and provides a simple path to remove sibling
      blocks when we coalesce nodes without having to scan through the free
      list for the right entry.
 * A statically-allocated 2 MB chunk of memory.
    * In a bare-metal embedded system, you'd probably just point this kind of
      allocator at your free data RAM.  In a larger system, you'd work with the
      OS to allocate more memory space; the general pattern of
      [jemalloc](https://people.freebsd.org/~jasone/jemalloc/bsdcan2006/jemalloc.pdf)
      and friends is to allocate 2 MB chunks at a time.
    * This means our toy implementation is missing some bits which would be
      required for a "real" allocator (management of the allocated chunks,
      searching across multiple allocated chunks), but for the purposes of
      playing with the buddy allocation, this should be sufficient.

