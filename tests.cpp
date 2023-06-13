#include "chess.h"
#include <cassert>
#include <iostream>
#include <random>

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

    print_bitboard(pawn_attacks<White>(brd));
}