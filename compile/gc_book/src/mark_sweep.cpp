#include "mark_sweep.hpp"

namespace tiny_gc {
    using std::runtime_error;

    void mark(vector<int64_t> &blocks, uint64_t r) {
        if (!static_cast<bool>(blocks[r + 1])) {
            blocks[r + 1] = static_cast<int64_t>(true);
            if (static_cast<bool>(blocks[r + 2])) {
                mark(blocks, static_cast<uint64_t>(blocks[r + 3]));
            }
        }
    }

    void mark_phrase(vector<int64_t> &blocks, const vector<uint64_t> &roots) {
        for (const auto r : roots) {
            mark(blocks, r);
        }
    }

    void sweep_phrase(vector<int64_t> &blocks, uint64_t &freelist) {
        uint64_t sweeping = 0;
        while (sweeping < blocks.size()) {
            if (static_cast<bool>(blocks[sweeping + 1])) {
                blocks[sweeping + 1] = static_cast<int64_t>(false);
            } else {
                if (freelist != null && sweeping == static_cast<uint64_t>(blocks[freelist]) + freelist) {
                    blocks[freelist] = blocks[freelist] + blocks[sweeping];
                } else {
                    blocks[sweeping + 1] = static_cast<int64_t>(freelist);
                    freelist = sweeping;
                }
            }
            sweeping += static_cast<uint64_t>(blocks[sweeping]);
        }
    }

    void mark_sweep(vector<int64_t> &blocks, const vector<uint64_t> &roots, uint64_t &freelist) {
        mark_phrase(blocks, roots);
        sweep_phrase(blocks, freelist);
    }

    void sweep_phrase_multi_lists(vector<int64_t> &blocks, const vector<uint64_t> &roots, vector<uint64_t> &freelists) {
        uint64_t sweeping = 0;
        while (sweeping < blocks.size()) {
            if (static_cast<bool>(blocks[sweeping + 1])) {
                blocks[sweeping + 1] = static_cast<int64_t>(false);
            } else {
                uint64_t index = static_cast<uint64_t>(blocks[sweeping]) / 4 - 1;
                if (index >= freelists.size() - 1) {
                    index = freelists.size() - 1;
                }
                uint64_t &freelist = freelists[index];
                blocks[sweeping + 1] = static_cast<int64_t>(freelist);
                freelist = sweeping;
            }
            sweeping += static_cast<uint64_t>(blocks[sweeping]);
        }
    }

    void mark_sweep_multi_lists(vector<int64_t> &blocks, const vector<uint64_t> &roots, vector<uint64_t> &freelists) {
        mark_phrase(blocks, roots);
        sweep_phrase_multi_lists(blocks, roots, freelists);
    }

    void mark_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap, uint64_t r) {
        let index = r / 64;
        let offset = r % 64;
        let test_num = (uint64_t{1} << offset);
        if ((bitmap[index] & (test_num << offset)) == 0) {
            bitmap[index] |= test_num;
            if (static_cast<bool>(blocks[r + 1])) {
                mark_bitmap(blocks, bitmap, static_cast<uint64_t>(blocks[r + 2]));
            }
        }
    }

    void mark_phrase_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap, const vector<uint64_t> &roots) {
        for (const auto r: roots) {
            mark_bitmap(blocks, bitmap, r);
        }
    }

    void sweep_phrase_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap, uint64_t &freelist) {
        uint64_t sweeping = 0u;
        uint64_t index = 0u;
        uint64_t offset = 0u;
        while (sweeping < blocks.size()) {
            let test_num = uint64_t{1} << offset;
            if ((bitmap[index] & test_num) == 0) {
                blocks[sweeping + 1] = static_cast<uint64_t>(freelist);
                freelist = sweeping;
            }
            let sweep_size = static_cast<uint64_t>(blocks[sweeping]);
            offset = (offset + sweep_size) % 64;
            index += (offset + sweep_size) / 64;
            sweeping += sweep_size;
        }
        for (auto &bit: bitmap) {
            bit = 0;
        }
    }

    void mark_sweep_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap,
                           const vector<uint64_t> &roots, uint64_t &freelist) {
        mark_phrase_bitmap(blocks, bitmap,  roots);
        sweep_phrase_bitmap(blocks, bitmap, freelist);
    }

    uint64_t pick_chunk(vector<int64_t> &blocks, uint64_t &freelist, uint64_t size) {
        uint64_t current = freelist;
        uint64_t last = null;
        while (current < blocks.size()) {
            if (static_cast<uint64_t>(blocks[current]) == size) {
                const uint64_t res = current;
                if (current == freelist) {
                    freelist = static_cast<uint64_t>(blocks[current + 1]);
                } else {
                    blocks[last + 1] = blocks[current + 1];
                }
                blocks[current + 1] = static_cast<int64_t>(false);
                return res;
            } else if (static_cast<uint64_t>(blocks[current]) > size) {
                const uint64_t res = current;
                const uint64_t remain = static_cast<uint64_t>(blocks[current]) - size;
                if (current == freelist) {
                    freelist = current + size;
                } else {
                    blocks[last + 1] = static_cast<int64_t>(current + size);
                }
                blocks[current] = static_cast<int64_t>(size);
                blocks[current + size] = static_cast<int64_t>(remain);
                blocks[current + size + 1] = blocks[current + 1];
                blocks[current + 1] = static_cast<int64_t>(false);
                return res;
            } else {
                last = current;
                current = static_cast<uint64_t>(blocks[current + 1]);
            }
        }
        return null;
    }

    uint64_t new_obj(vector<int64_t> &blocks, uint64_t &freelist, uint64_t size) {
        if (size == 0) throw runtime_error("empty alloc");
        const auto chunk = pick_chunk(blocks, freelist,
                                      static_cast<uint64_t>(ceil(static_cast<float>(size + 3) / 4.0)) * 4);
        if (chunk == null) {
            throw runtime_error("alloc failed");
        } else {
            return chunk;
        }
    }
}
