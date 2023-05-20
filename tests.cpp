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

constexpr Board mailbox_to_bitboard(const Mailbox& brd) {
    std::array<u64, 12> bitboards;
    std::fill(bitboards.begin(), bitboards.end(), 0ull);

    for (auto i = 0; i < 64; i++) {
        if (brd[i] != Square::Empty) {
            bitboards[brd[i]] |= (msb >> i);
        }
    }

    return Board(bitboards[0], bitboards[1], bitboards[2], bitboards[3],
                 bitboards[4], bitboards[5], bitboards[6], bitboards[7],
                 bitboards[8], bitboards[9], bitboards[10], bitboards[11]);
}

Board random_board() {
    Mailbox brd;
    std::fill(brd.begin(), brd.end(), Square::Empty);

    std::tuple<Square, int> pieces[] = {
        {Square::wp, randint(0, 8)}, {Square::wn, 1},
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

    std::shuffle(brd.begin(), brd.end(), std::default_random_engine(0));

    // remove pawns on 1st and 8th ranks
    for (auto i = 0; i < 8; i++) {
        if (is_pawn(brd[i])) {
            brd[i] = Square::Empty;
        }
    }
    for (auto i = 56; i < 64; i++) {
        if (is_pawn(brd[i])) {
            brd[i] = Square::Empty;
        }
    }

    return mailbox_to_bitboard(brd);
}

int main(int argc, char** argv) {
    while (1) {
        auto brd = random_board();

        assert(is_board_valid_debug(brd));

        // find index of white knight (guaranteed to be exactly one)
        auto wn_idx = std::countl_zero(brd.wn());

        u64 attacks = knight_attacks(brd, wn_idx);
        // bit_loop(attacks);

        if (attacks) {
            // TODO use bit_loop instead of just checking first value
            auto att_idx = std::countl_zero(attacks);

            auto brd_copy = brd;

            make_wn_move(brd, wn_idx, att_idx);
            make_wn_move_avx(brd_copy, wn_idx, att_idx);

            // failing test case :(
            assert(brd == brd_copy);
        }

        // print_bitboard(brd.wn());
        print_board(brd);
    }
}