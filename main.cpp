#include "chess.h"
#include <cassert>
#include <iostream>

void use_board(Board& brd);

void iterate_knight_moves(Board& brd) {
    // if there is a forward pawn move (no capture)
    // then that means that there is either a pawn
    // directly behind the target square, or two
    // squares behind.

    // it is not possible for a pawn to be 2 away
    // from the target square AND for there to be
    // a pawn in between the target square and the
    // original square.

    // the possibilities are as follows:

    // 10 - 1 forward
    // 11 - 1 forward
    // 01 - 2 forward

    // this means to find the index of the from square
    // when iterating over the moves, you need both bits

    // now iterate over possible moves for that knight

    for (auto n_idx : BitIterator(brd.wn())) {
        u64 atks = knight_attacks<White>(brd, n_idx);
        for (auto atk_idx : BitIterator(atks)) {
            auto undo =
                make_move_undoable<White, Knight>(brd, Move(n_idx, atk_idx));

            // use_board(brd);

            undo_move(brd, undo);
        }
    }
}

// now we just need some benchmarks for this

int main(int argc, char** argv) {
    auto brd = Board::starting_position();

    auto n_idx = 31;

    brd.wn() |= MSB64 >> n_idx;
    brd.white() |= MSB64 >> n_idx;
    brd.occup() |= MSB64 >> n_idx;
    assert(is_board_valid_debug(brd));

    for (auto n_idx : BitIterator(brd.wn())) {
        u64 atks = knight_attacks<White>(brd, n_idx);
        for (auto atk_idx : BitIterator(atks)) {
            auto undo =
                make_move_undoable<White, Knight>(brd, Move(n_idx, atk_idx));

            print_board(brd);

            assert(is_board_valid_debug(brd));

            undo_move(brd, undo);

            assert(is_board_valid_debug(brd));
        }
    }

    // // iterate over indexes of white knights
    // for (u64 wn2 = brd.wn(); wn2;) {
    //     auto n_idx = std::countl_zero(wn2);

    //     // now iterate over possible moves for that knight
    //     u64 atks = knight_attacks<White>(brd, n_idx);
    //     while (atks) {
    //         auto atk_idx = std::countl_zero(atks);

    //         // unmake move instead of copy?
    //         // iterate through all knight attacks at once?
    //         // remove some unnecessary shifts by not dealing with index?

    //         auto undo =
    //             make_move_undoable<White, Knight>(brd, Move(n_idx, atk_idx));

    //         print_board(brd);

    //         assert(is_board_valid_debug(brd));

    //         undo_move(brd, undo);

    //         assert(is_board_valid_debug(brd));

    //         atks &= ~(MSB64 >> atk_idx);
    //     }

    //     wn2 &= ~(MSB64 >> n_idx);
    // }
}
