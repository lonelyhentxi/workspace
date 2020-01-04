#ifndef GC_BOOK_MARK_SWEEP_HPP
#define GC_BOOK_MARK_SWEEP_HPP

#include<iostream>
#include<vector>
#include<limits>
#include<cassert>
#include<cmath>
#include<cstdint>
#include<stdexcept>
#include "defines.hpp"

namespace tiny_gc {
    using std::vector;
    using std::runtime_error;

    void mark(vector<int64_t> &blocks, uint64_t r);

    void mark_phrase(vector<int64_t> &blocks, const vector<uint64_t> &roots);

    void sweep_phrase(vector<int64_t> &blocks, uint64_t &freelist);

    void mark_sweep(vector<int64_t> &blocks, const vector<uint64_t> &roots, uint64_t &freelist);

    void sweep_phrase_multi_lists(vector<int64_t> &blocks, const vector<uint64_t> &roots, vector<uint64_t> &freelist);

    void mark_sweep_multi_lists(vector<int64_t> &blocks, const vector<uint64_t> &roots, vector<uint64_t> &freelist);

    void mark_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap, uint64_t r);

    void mark_phrase_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap, const vector<uint64_t> &roots);

    void sweep_phrase_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap, uint64_t &freelist);

    void mark_sweep_bitmap(vector<int64_t> &blocks, vector<uint64_t> &bitmap,
                           const vector<uint64_t> &roots, uint64_t &freelist);

    uint64_t pick_chunk(vector<int64_t> &blocks, uint64_t &freelist, uint64_t size);

    uint64_t new_obj(std::vector<int64_t> &blocks, uint64_t &freelist, uint64_t size);

    uint64_t lazy_sweep(vector<int64_t> &blocks, uint64_t &sweeping, uint64_t size);

    uint64_t new_obj_lazy_sweep(vector<int64_t> &blocks, const vector<uint64_t> &roots,
            uint64_t &sweeping, uint64_t size);
}

#endif //GC_BOOK_MARK_SWEEP_HPP
