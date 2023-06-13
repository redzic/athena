#pragma once

#include "chess.h"

#include <algorithm>
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

constexpr Board mailbox_to_bitboard(const Mailbox& brd) {
    std::array<u64, 12> bitboards;
    std::fill(bitboards.begin(), bitboards.end(), 0ull);

    for (auto i = 0; i < 64; i++) {
        if (brd[i] != Square::Empty) {
            bitboards[brd[i]] |= (1ull << i);
        }
    }

    return Board(bitboards[0], bitboards[1], bitboards[2], bitboards[3],
                 bitboards[4], bitboards[5], bitboards[6], bitboards[7],
                 bitboards[8], bitboards[9], bitboards[10], bitboards[11]);
}

Board random_board() {
    Mailbox brd;
    std::fill(brd.begin(), brd.end(), Square::Empty);

    std::tuple<Square, int> pieces[] = {{Square::WhitePawn, randint(0, 8)},
                                        {Square::WhiteKnight, 1},
                                        {Square::WhiteRook, randint(0, 2)},
                                        {Square::WhiteBishop, randint(0, 2)},
                                        {Square::WhiteQueen, randint(0, 1)},
                                        {Square::WhiteKing, 1},
                                        {Square::BlackPawn, randint(0, 8)},
                                        {Square::BlackKnight, randint(0, 2)},
                                        {Square::BlackRook, randint(0, 2)},
                                        {Square::BlackBishop, randint(0, 2)},
                                        {Square::BlackQueen, randint(0, 1)},
                                        {Square::BlackKing, 1}};

    auto output_idx = 0;

    for (const auto& [piece, count] : pieces) {
        for (auto i = 0; i < count; i++)
            brd[output_idx++] = piece;
    }

    assert(output_idx <= 64);

    std::shuffle(brd.begin(), brd.end(),
                 std::default_random_engine(std::random_device()()));

    auto bb = mailbox_to_bitboard(brd);

    // remove pawns on 1st and 8th ranks

    u64 removed_pawns = (bb.bp() | bb.wp()) & (RANK1 | RANK8);

    bb.white() &= ~(bb.wp() & (RANK1 | RANK8));
    bb.black() &= ~(bb.bp() & (RANK1 | RANK8));
    bb.wp() &= ~(RANK1 | RANK8);
    bb.bp() &= ~(RANK1 | RANK8);
    bb.occup() &= ~removed_pawns;

    return bb;
}
