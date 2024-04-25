#ifndef MM_EMULATOR_H
#define MM_EMULATOR_H

#include <stdint.h>
#include <cstring>
#include <queue>

/*
    a read operation , each read operation only contains one read word, so a read request from the CPU
    can be decomposed into multiple read operations, depending on how many words the request need to read
*/
struct mm_rresp_t
{
    uint64_t id;             // each read request has a unique ID, so all read operations associated to the same read request have the same ID
    std::vector<char> data;  // the data (i.e., one word) read from the memory
    bool last;               // whether this read operation is the last one for a read request

    /* create a read operation */
    mm_rresp_t(uint64_t id, std::vector<char> data, bool last)
    {
        this->id   = id;
        this->data = data;
        this->last = last;
    }

    /* create a default read operation */
    mm_rresp_t()
    {
        this->id   = 0;
        this->last = false;
    }
};

/*
    the memory module used in verilator simulation
    important term used in this module:
        request: the request received from CPU
        operation: each request can be decompesed into severl operations in the memory,
                   an operation only takes care of one word
*/
class mm_magic_t
{
public:
    /* constructor */
    mm_magic_t(size_t size, size_t word_size);

    /* destructor */
    ~mm_magic_t();

    /* get the pointer to the whole actual memory data */
    char* get_data() { return data; }

    /* get the size of this memory */
    size_t get_size() { return size; }

    //================================================================================

    // --------------------
    // write

    /* whether the memory is ready for receiving write address */
    bool aw_ready() { return !store_inflight; }

    /* whether the memory is ready for receiving write data */
    bool w_ready() { return store_inflight; }

    /* whether the memory has completed write requests */
    bool b_valid() { return !bresp.empty(); }

    /* get the next completed writed request ID, 0 if no completed write requests yet */
    uint64_t b_id() { return b_valid() ? bresp.front() : 0; }

    /*  */
    uint64_t b_resp() { return 0; }

    // --------------------
    // read

    /* whether the memory is ready for receiving read address (always ready) */
    bool ar_ready() { return true; }

    /* whether there has been completed read operations */
    bool r_valid() { return !rresp.empty(); }

    /*  */
    uint64_t r_resp() { return 0; }

    /* get the next completed (or in service) read request ID (0 if no completed read operations yet) */
    uint64_t r_id() { return r_valid() ? rresp.front().id: 0; }

    /* get the data pointer of the next completed read operation (dummy data if no completed read operations yet) */
    void *r_data() { return r_valid() ? &rresp.front().data[0] : &dummy_data[0]; }

    /* check if the next completed read operation is the last one of a read request */
    bool r_last() { return r_valid() ? rresp.front().last : false; }

    //================================================================================

    /* write a word into an address */
    void write(uint64_t addr, char *data);

    /* write parts of a word into an address, byte mask is specified by strb */
    void write(uint64_t addr, char *data, uint64_t strb, uint64_t size);

    /* read a word from an address */
    std::vector<char> read(uint64_t addr);

    //================================================================================

    /* interface to verilator simulation for one clock tick */
    void tick
    (
        bool reset,

        // CPU issue read request
        bool ar_valid,
        uint64_t ar_addr,
        uint64_t ar_id,
        uint64_t ar_size,
        uint64_t ar_len,

        // CPU issue write request
        bool aw_valid,
        uint64_t aw_addr,
        uint64_t aw_id,
        uint64_t aw_size,
        uint64_t aw_len,

        // CPU send write data
        bool w_valid,
        uint64_t w_strb,
        void *w_data,
        bool w_last,

        // CPU ready for receiving from memory
        bool r_ready,
        bool b_ready
    );

private:
    char* data;                    // the actual memory data
    size_t size;                   // size of the memory in bytes
    size_t word_size;              // word size in bytes

    bool store_inflight;           // if the memory is currently serving a write
    uint64_t store_addr;           // address for current write request
    uint64_t store_id;             // ID of the current write request
    uint64_t store_size;           // number of bytes to write in one write beat
    uint64_t store_count;          // count down the number of beats for current write request
    std::vector<char> dummy_data;  // dummy data
    std::queue<uint64_t> bresp;    // queue of completed write requests (for write ack to CPU), only store the wrtie request IDs
    std::queue<mm_rresp_t> rresp;  // queue of completed read operations (NOTE: not read request)
};

/* initialize memroy, i.e., load a file into the memory starting from address 0 */
void load_mem(char* mem, const char* fn);

#endif
