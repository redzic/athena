#include "chess.h"
#include <cassert>
#include <iostream>

int main(int argc, char** argv) {
    auto brd = Board::starting_position();

    auto n_idx = 31;

    brd.wn() |= msb >> n_idx;
    brd.white() |= msb >> n_idx;
    brd.occup() |= msb >> n_idx;
    assert(is_board_valid_debug(brd));

    // print_board(brd);

    make_move<White, Knight>(brd, n_idx, n_idx - 17);

    assert(is_board_valid_debug(brd));

    // assert(is_board_valid_debug(brd));

    // print_board(brd);

    // print_bitboard(brd.wn());
    print_board(brd);
    // bit_loop(brd.wn());
}
