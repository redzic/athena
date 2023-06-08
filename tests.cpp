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
            bitboards[brd[i]] |= (MSB64 >> i);
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

    // for (auto i = 0; i < 8; i++) {
    //     if (is_pawn(brd[i])) {
    //         brd[i] = Square::Empty;
    //     }
    // }
    // for (auto i = 56; i < 64; i++) {
    //     if (is_pawn(brd[i])) {
    //         brd[i] = Square::Empty;
    //     }
    // }

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

bool bitboard_assert_eq(const Board& b1, const Board& b2) {
    bool is_eq = true;
    for (auto i = 0; i < 12; i++) {
        if (b1.bitboards[i] != b2.bitboards[i]) {
            std::cout << "bitboard index " << i << " (" << CHAR_PIECE_LOOKUP[i]
                      << ") differs\n";
            is_eq = false;
        }
    }
    return is_eq;
}

int main(int argc, char** argv) {
    // while (1) {
    auto brd = random_board();

    assert(is_board_valid_debug(brd));

    // print_bitboard(brd.wn());
    print_board(brd);
    // }

    print_bitboard(pawns_atk<White>(brd.wp(), brd.occup(), brd.black()));
}