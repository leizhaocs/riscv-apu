#include "mm.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <string>
#include <cassert>

/* constructor */
mm_magic_t::mm_magic_t(size_t size, size_t word_size)
    : data(new char[size])
    , size(size)
    , word_size(word_size)
    , store_inflight(false)
{
    dummy_data.resize(word_size);
}

/* destructor */
mm_magic_t::~mm_magic_t()
{
  delete [] data;
}

/* write a word into an address */
void mm_magic_t::write(uint64_t addr, char *data) {
    addr %= this->size;

    char* base = this->data + addr;
    memcpy(base, data, word_size);
}

/* write parts of a word into an address, byte mask is specified by strb */
void mm_magic_t::write(uint64_t addr, char *data, uint64_t strb, uint64_t size)
{
    strb &= ((1L << size) - 1) << (addr % word_size);
    addr %= this->size;

    char *base = this->data + addr;
    for (int i = 0; i < word_size; i++)
    {
        if (strb & 1)
            base[i] = data[i];
        strb >>= 1;
    }
}

/* read a word from an address */
std::vector<char> mm_magic_t::read(uint64_t addr)
{
    addr %= this->size;

    char *base = this->data + addr;
    return std::vector<char>(base, base + word_size);
}

/* interface to verilator simulation for one clock tick */
void mm_magic_t::tick(
    bool reset,         // reset signal

    // CPU issue read request
    bool ar_valid,      // CPU has put read address available on bus
    uint64_t ar_addr,   // read address
    uint64_t ar_id,     // read request ID
    uint64_t ar_size,   // 
    uint64_t ar_len,    // number of words to read

    // CPU issue write request
    bool aw_valid,      // CPU has put write address available on bus
    uint64_t aw_addr,   // write address
    uint64_t aw_id,     // write request ID
    uint64_t aw_size,   // log of (number of bytes to write in one write beat)
    uint64_t aw_len,    // number of write beats of the write request

    // CPU send write data
    bool w_valid,       // CPU has put write data available on bus
    uint64_t w_strb,    // byte mask to write
    void *w_data,       // data to write into memory
    bool w_last,        // whether this is the last write beat of the write request

    // CPU ready for receiving from memory
    bool r_ready,       // CPU is ready to receive read data
    bool b_ready        // CPU is ready to receive write ack
)
{
    bool ar_fire = !reset && ar_valid && ar_ready();  // whether ready to receive read address (CPU has put read address on bus)
    bool r_fire  = !reset && r_valid() && r_ready;    // whether read has been completed (CPU is also ready to receive the ack)

    bool aw_fire = !reset && aw_valid && aw_ready();  // whether ready to receive write address (CPU has put write address on bus)
    bool w_fire  = !reset && w_valid && w_ready();    // whether ready to receive write data
    bool b_fire  = !reset && b_valid() && b_ready;    // whether write has been completed (CPU is also ready to receive the ack)

    // ready to receive read address
    if (ar_fire)
    {
        // only read in the granularity of word, ignore offset inside word
        uint64_t start_addr = (ar_addr / word_size) * word_size;

        // decompose read request into multiple read operations
        for (size_t i = 0; i <= ar_len; i++)
        {
            auto dat = read(start_addr + i * word_size);
            rresp.push(mm_rresp_t(ar_id, dat, i == ar_len));
        }
    }

    // ready to receive write address
    if (aw_fire)
    {
        store_addr     = aw_addr;       // store write address
        store_id       = aw_id;         // store wirte request ID
        store_size     = 1 << aw_size;  // store number of bytes to write in one write beat
        store_count    = aw_len + 1;    // store number of write beats in total for this write (used for counting down)
        store_inflight = true;          // mark that we have started a write service (ready to receive write data)
    }

    // ready to receive write data
    if (w_fire)
    {
        // perform one write beat
        write(store_addr, (char*)w_data, w_strb, store_size);
        store_addr += store_size;
        store_count--;

        // if no write beat remaining, this write request has been finished
        if (store_count == 0)
        {
            store_inflight = false;
            bresp.push(store_id);
            assert(w_last);
        }
    }

    if (b_fire)
        bresp.pop();

    if (r_fire)
        rresp.pop();

    if (reset)
    {
        while (!bresp.empty())
            bresp.pop();
        while (!rresp.empty())
            rresp.pop();
    }
}

/* initialize memroy, i.e., load a file into the memory starting from address 0 */
void load_mem(char* mem, const char* fn)
{
    int start = 0;
    std::ifstream in(fn);
    if (!in)
    {
        std::cerr << "could not open " << fn << std::endl;
        exit(EXIT_FAILURE);
    }

    std::string line;
    while (std::getline(in, line))
    {
        #define parse_nibble(c) ((c) >= 'a' ? (c)-'a'+10 : (c)-'0')
        for (int i = line.length()-2, j = 0; i >= 0; i -= 2, j++)
        {
            mem[start + j] = (parse_nibble(line[i]) << 4) | parse_nibble(line[i+1]);
        }
    start += line.length()/2;
    }
}
