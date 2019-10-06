#include <algorithm>  // for `std::max`
#include <cassert>  // for `assert`
#include <cstddef>  // for `std::max_align_t`
#include <stdint.h>  // for fixed-size integers
#include <unistd.h>  // for `brk`/`sbrk`

namespace {

struct ListNode;

struct BlockHeader {
    // Used to track the originally allocated size, which is necessary to know
    // what 'order' this block belongs to.
    size_t allocated_size = 0;

    // Used for heap debugging / allocation tracking, store the alloc()
    // caller's address when in use, and nullptr when freed.
    void* allocated_from = nullptr;

    // Returns the address of the data which immediately follows this header.
    void* data() {
        return reinterpret_cast<char*>(this) + sizeof(*this);
    }

    ListNode* transmute();
};

// In order to not disrupt the alignment of the actual data we return to the
// caller, we need to keep this structure padded out to the appropriate
// alignment size.
static_assert(sizeof(BlockHeader) == alignof(std::max_align_t), "");

struct ListNode {
    ListNode* prev = this;
    ListNode* next = this;

    // Inserts an element at the "back" of the list.
    void push(ListNode* new_entry) {
        // The `new_entry` should not already be part of a list.
        assert(!new_entry->prev && !new_entry->next);

        prev->next = new_entry;
        new_entry->prev = prev;
        new_entry->next = this;
        prev = new_entry;
    }

    // Pops the "back" entry off of a list, or returns nullptr if empty.
    ListNode* pop() {
        if (prev == this) {
            return nullptr;
        }

        ListNode* back = prev;
        back->remove();
        return back;
    }

    // Removes this node from whatever list it's present in.
    // Note that thanks to the circularly-linked nature of our lists, we don't
    // actually have to know which list we belong to.
    void remove() {
        // This node must already be part of a list.
        assert(prev && next);

        prev->next = next;
        next->prev = prev;

        // While this part is not strictly necessary, it allows our assert()s
        // above to catch list corruptions earlier, aiding debugging.
        prev = next = nullptr;
    }

    BlockHeader* transmute() {
        assert(!prev && !next);

        auto block = reinterpret_cast<BlockHeader*>(this);
        block->allocated_size = 0;
        block->allocated_from = nullptr;
        return block;
    }
};

ListNode* BlockHeader::transmute() {
    auto node = reinterpret_cast<ListNode*>(this);
    node->prev = nullptr;
    node->next = nullptr;
    return node;
}

// A block is either used as a BlockHeader (while allocated), or as a ListNode
// (in the relevant order's free list, when free).  So, we can't allocate
// anything smaller than one of those two objects.
constexpr size_t kMinimumAllocation = std::max(sizeof(BlockHeader), sizeof(ListNode));

constexpr size_t log2(const size_t n) {
    size_t result = 0;
    size_t max = 1;
    while (n > max) {
        result++;
        // We have to be a little careful here, because blind doubling could
        // cause us to overflow if `n` is close to size_t's maximum value.
        if (max > std::numeric_limits<size_t>::max() / 2) {
            return result;
        }
        max *= 2;
    }
    return result;
}
static_assert(log2(1) == 0, "");
static_assert(log2(2) == 1, "");
static_assert(log2(3) == 2, "");
static_assert(log2(4) == 2, "");
static_assert(log2(5) == 3, "");

// This class is the primary purpose of this entire enterprise: it effectively
// represents one "chunk" of RAM which would be managed within the larger scope
// of a full-blown allocator.
//
// We use a pure powers-of-two buddy allocation algorithm internally, which has
// relatively high internal fragmentation vs. something more intelligent like
// you might see in jemalloc.
class HeapChunk {
public:
    // Using a fixed chunk size to emulate one smaller arena of an otherwise larger
    // allocator, this is the total RAM space that we'll dole out.
    static constexpr size_t kChunkSize = 2 * 1024 * 1024;

    HeapChunk() {
        ListNode* root = reinterpret_cast<ListNode*>(heap_base);
        free_lists[kMaximumOrder].push(root);
    }

    // You could imagine connecting these up to the global symbols.
    void* malloc(const size_t request) {
        const auto required_size = std::max(request + sizeof(BlockHeader), kMinimumAllocation);
        const auto required_order = log2(required_size) - log2(kMinimumAllocation);

        // Find the smallest free block which can fit this allocation.
        size_t order = required_order;
        ListNode* node = nullptr;
        for (; order <= kMaximumOrder; order++) {
            node = free_lists[order].pop();
            if (node) {
                break;
            }
        }

        // We either found a block that can fit this allocation by now, or we
        // cannot service this request.
        if (!node) {
            return nullptr;
        }

        auto index = index_for_node(node, order);
        if (order < kMaximumOrder) {
            // If our parent was previously split, they aren't now, because
            // we've been allocated.  (The root node doesn't have a parent.)
            toggle_split(parent(index));
        }

        // While the order of the block we're holding is larger than we
        // require, split the block in two, keeping the left child for
        // ourselves and placing the right child onto its free list.
        while (order > required_order) {
            const auto index = index_for_node(node, order);
            toggle_split(index);
            order--;

            auto right = node_for_index(right_child(index), order);
            free_lists[order].push(right);

            node = node_for_index(left_child(index), order);
        }

        auto block = node->transmute();
        block->allocated_size = required_size;
        block->allocated_from = __builtin_return_address(0);
        return block->data();
    }

    void free(void* ptr) {
        if (!ptr) {
            return;
        }

        BlockHeader* block = reinterpret_cast<BlockHeader*>(reinterpret_cast<char*>(ptr) - sizeof(BlockHeader));
        assert(block->allocated_size <= kChunkSize);
        assert(block->allocated_from != nullptr);

        auto order = log2(block->allocated_size) - log2(kMinimumAllocation);
        auto node = block->transmute();
        auto index = index_for_node(node, order);

        while (order < kMaximumOrder && is_split(parent(index))) {
            // We're freeing `node` and our parent is "split," meaning our
            // sibling must already be free (`node` was the allocated one).
            // That means we can coalesce these siblings.
            //
            // First, remove the sibling from its relevant free list.
            auto sibling_node = node_for_index(sibling(index), order);
            sibling_node->remove();

            // Update our information to point at the parent, which is no
            // longer split.
            index = parent(index);
            toggle_split(index);
            order++;
            node = node_for_index(index, order);
        }

        // Either we're holding the root node, or our sibling is allocated, so
        // just return this node to its respective free list.
        free_lists[order].push(node);

        // This means that our parent (assuming we're not the root) is now
        // split, since we're free and couldn't coalesce with our sibling.
        if (order < kMaximumOrder) {
            toggle_split(parent(index));
        }
    }

private:
    // Relationship between data pointers and binary tree node indices.
    //
    // The "node is split" binary tree looks something like this:
    //      kMaximumOrder ->       0
    //  kMaximumOrder - 1 ->    1     2
    //  kMaximumOrder - 2 ->  3   4 5   6
    //
    // The bitmap representation looks like:
    //     [0][1][2][3][4][5][6]
    // Where for a node with index i:
    //     left child:  2i + 1
    //     right child: 2i + 2
    //     parent:      floor((i - 1) / 2)
    //
    // The matching block addresses look like:
    //     +---------------+
    //     |       0       | 0: 0b00
    //     +-------+-------+
    //     |   1   |   2   | 1: 0b00 2: 0b10
    //     +---+---+---+---+
    //     | 3 | 4 | 5 | 6 | 3: 0b00 4: 0b01 5: 0b10 6: 0b11
    //     +---+---+---+---+

    // With a known `order` and start address of the block, get the index.
    size_t index_for_node(ListNode* node, size_t order) {
        const auto tree_depth = kMaximumOrder - order;
        const auto first_index = (1 << tree_depth) - 1;
        const auto block_size = (1 << order) * kMinimumAllocation;
        return first_index + (reinterpret_cast<char*>(node) - heap_base) / block_size;
    }

    // With a known `order` and `index`, get the relevant node pointer.
    ListNode* node_for_index(size_t index, size_t order) {
        const auto tree_depth = kMaximumOrder - order;
        const auto first_index = (1 << tree_depth) - 1;
        const auto block_size = (1 << order) * kMinimumAllocation;
        return reinterpret_cast<ListNode*>(heap_base + (index - first_index) * block_size);
    }

    size_t parent(size_t index) const {
        return (index - 1) / 2;
    }

    size_t left_child(size_t index) const {
        return 2 * index + 1;
    }

    size_t right_child(size_t index) const {
        return 2 * index + 2;
    }

    size_t sibling(size_t index) const {
        return ((index - 1) ^ 1) + 1;
    }

    bool is_split(size_t index) const {
        return (split_nodes[index / 8] >> (index % 8)) & 1;
    }

    void toggle_split(size_t index) {
        split_nodes[index / 8] ^= (1 << (index % 8));
    }

    // Taking an allocation of `kMinimumAllocation` bytes to be "order 0," the
    // maximum order can be determined by the number of doublings between
    // `kMinimumAllocation` and the largest possible allocaiton.
    static constexpr size_t kMaximumOrder = log2(kChunkSize) - log2(kMinimumAllocation);

    // The list of free blocks we have for each order.
    ListNode free_lists[kMaximumOrder + 1] = {};

    // The bit-map telling us which parent nodes are split.
    //
    // We don't have to track the 'order 0' blocks, since they can't be split
    // by definition.  That leaves 2**(max order) - 1 bits that need allocation
    // (for a binary tree of depth 'max order').
    uint8_t split_nodes[(1 << kMaximumOrder) / 8] = {};

    // The address space we'll actually dole out for allocations.
    //
    // This is actually not _quite_ right, since something like jemalloc would
    // embed the control structures at the front of the chunk and we keep them
    // separately here (meaning sizeof(*this) is actually a bit larger than
    // kChunkSize, but for a learning-only project where we're only working
    // with a single chunk, that seems fine.
    alignas(std::max_align_t) char heap_base[kChunkSize] = {};
};

}  // namespace

// Unit Testing
//
// These are far from exhaustive; if you're going to use this for anything
// real, you should really amp up the coverage.
#include <iostream>
#include <random>
#include <set>
#include <vector>

void test_lists() {
    ListNode list;
    ListNode elem_a, elem_b, elem_c;
    elem_a.prev = elem_b.prev = elem_c.prev = nullptr;
    elem_a.next = elem_b.next = elem_c.next = nullptr;

    list.push(&elem_a);
    assert(&elem_a == list.pop());
    assert(nullptr == list.pop());

    list.push(&elem_b);
    list.push(&elem_c);
    assert(&elem_c == list.pop());
    assert(&elem_b == list.pop());
    assert(nullptr == list.pop());

    list.push(&elem_b);
    list.push(&elem_c);
    list.push(&elem_a);
    elem_c.remove();
    assert(&elem_a == list.pop());
    assert(&elem_b == list.pop());
    assert(nullptr == list.pop());
}

// Verifies we get exactly `n` allocations of `size` before OOM.
void check_oom_after(HeapChunk& heap, size_t n, size_t size) {
    std::cout << "Trying " << n << " allocs of " << size << "\n";
    std::set<void*> allocations;

    for (size_t i = 0; i < n; i++) {
        auto ptr = heap.malloc(size);
        std::cout << "\t[" << i << "] " << ptr << "\n";
        assert(ptr != nullptr);

        // Verify we didn't get a duplicate address.
        const auto pair = allocations.insert(ptr);
        assert(pair.second);
    }

    auto ptr = heap.malloc(size);
    std::cout << "\t[extra] " << ptr << "\n";
    assert(ptr == nullptr);
}

// Pseudo-random stress test.  This would be much better written using a
// property-based testing library, but this is just a quick and dirty
// verification.
//
// Importantly, this doesn't really attempt to make any verifications about
// fragmentation / coalescing behavior.
void stress_test() {
    std::cout << "=== Stress Test ===\n";
    constexpr size_t kSteps = 100000;

    struct Allocation {
        void* ptr = nullptr;
        size_t size = 0;
    };
    std::vector<Allocation> allocations;

    std::random_device rd;
    std::default_random_engine gen(rd());
    std::uniform_int_distribution<> action(0, 4);
    std::uniform_int_distribution<> small_alloc(0, 512);
    std::uniform_int_distribution<> medium_alloc(32 * 1024, 512 * 1024);
    std::uniform_int_distribution<> large_alloc(1 * 1024 * 1024, HeapChunk::kChunkSize - sizeof(BlockHeader));

    HeapChunk heap;

    const auto test_allocation = [&] (std::uniform_int_distribution<>& size) {
        const size_t request = size(gen);
        const auto allocation = Allocation {
            .ptr = heap.malloc(request),
            .size = request,
        };
        if (!allocation.ptr) {
            // n.b. we should probably check if this is expected/reasonable
            std::cout << "\toom\n";
            return;
        }
        std::cout << "Allocated " << allocation.ptr << " [" << request << "]\n";
        for (const auto& other : allocations) {
            // Check that the allocations do not overlap.
            const auto comes_before = ((char*)allocation.ptr + allocation.size) < other.ptr;
            const auto comes_after = ((char*)other.ptr + other.size) < allocation.ptr;
            assert(comes_before || comes_after);
        }
        allocations.push_back(allocation);
    };

    // Take a bunch of random allocate/free actions.
    for (size_t step = 0; step < kSteps; step++) {
        const auto selection = action(gen);
        switch (allocations.empty() ? selection & 0x3 : selection) {
            case 0:
                test_allocation(small_alloc);
                break;
            case 1:
                test_allocation(medium_alloc);
                break;
            case 2:
                test_allocation(large_alloc);
                break;
            case 4: {
                std::uniform_int_distribution<> index(0, allocations.size() - 1);
                const auto target = index(gen);
                const auto& allocation = allocations[target];
                std::cout << "Freeing " << allocation.ptr << " [" << allocation.size << "]\n";
                heap.free(allocation.ptr);
                allocations.erase(allocations.begin() + target);
                break;
            }
        }
    }

    auto total_allocated = 0;
    for (const auto& alloc : allocations) {
        total_allocated += alloc.size;
    }
    std::cout << "Total allocated at test end: " << total_allocated << "\n";

    // Free everything.
    for (const auto& alloc : allocations) {
        std::cout << "Freeing " << alloc.ptr << " [" << alloc.size << "]\n";
        heap.free(alloc.ptr);
    }

    // Final check that the heap successfully coalesced all the things.
    check_oom_after(heap, 1, HeapChunk::kChunkSize - sizeof(BlockHeader));
}

int main() {
    test_lists();

    size_t expected_n = 1;
    size_t block_size = HeapChunk::kChunkSize;
    while (block_size >= kMinimumAllocation) {
        HeapChunk heap;
        check_oom_after(heap, expected_n, block_size - sizeof(BlockHeader));

        block_size /= 2;
        expected_n *= 2;
    }

    stress_test();
}
