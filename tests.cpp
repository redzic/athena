#include "chess.h"
#include <cassert>
#include <iostream>
#include <random>

int randint(int min, int max) {
    std::random_device rd; // Only used once to initialise (seed) engine
    std::mt19937 rng(
        rd()); // Random-number engine used (Mersenne-Twister in this case)
    std::uniform_int_distribution<int> uni(min, max); // Guaranteed unbiased
    return uni(rng);
}

// generate random board position... ugh...

using Mailbox = std::array<Square, 64>;

Board mailbox_to_bitboard(const Mailbox& brd) {
    Board bb(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

    for (auto i = 0; i < 64; i++) {
        if (brd[i] != Square::Empty) {
            bb.bitboards[brd[i]] |= (msb >> i);
        }
    }

    return bb;
}

Board random_board() {
    Mailbox brd;
    std::fill(brd.begin(), brd.end(), Square::Empty);

    std::tuple<Square, int> pieces[] = {
        {Square::wp, randint(0, 8)}, {Square::wn, randint(0, 2)},
        {Square::wr, randint(0, 2)}, {Square::wb, randint(0, 2)},
        {Square::wq, randint(0, 1)}, {Square::wk, 1},
        {Square::bp, randint(0, 8)}, {Square::bn, randint(0, 2)},
        {Square::br, randint(0, 2)}, {Square::bb, randint(0, 2)},
        {Square::bq, randint(0, 1)}, {Square::bk, 1}};

    auto output_idx = 0;

    for (const auto& [piece, count] : pieces) {
        for (auto i = 0; i < count; i++)
            brd[output_idx++] = piece;
    }

    assert(output_idx <= 64);

    // remove pawns

    std::shuffle(brd.begin(), brd.end(), std::default_random_engine(0));

    return mailbox_to_bitboard(brd);
}

int main(int argc, char** argv) {
    // while (1) {
    auto brd = random_board();

    assert(is_board_valid_debug(brd));

    // print_bitboard(brd.wn());
    print_board(brd);
    bit_loop(brd.wn());
    // }
}