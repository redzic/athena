#include "chess.h"
#include <cassert>
#include <iostream>

// void use_board(Board& brd);

// void iterate_moves(Board& brd) {
//     // iterate over indexes of white knights
//     for (u64 wn2 = brd.wn(); wn2;) {
//         auto n_idx = std::countl_zero(wn2);

//         // now iterate over possible moves for that knight
//         u64 atks = knight_attacks<White>(brd, n_idx);
//         while (atks) {
//             auto atk_idx = std::countl_zero(atks);

//             // unmake move instead of copy?
//             // iterate through all knight attacks at once?
//             // remove some unnecessary shifts by not dealing with index?
//             auto brd_copy = brd;
//             make_move<White, Knight>(brd_copy, n_idx, atk_idx);

//             // assert(is_board_valid_debug(brd_copy));

//             use_board(brd_copy);
//             // print_board(brd_copy);

//             atks &= ~(MSB64 >> atk_idx);
//         }

//         wn2 &= ~(MSB64 >> n_idx);
//     }

//     // assert(is_board_valid_debug(brd));
// }

// now we just need some benchmarks for this

int main(int argc, char** argv) {
    auto brd = Board::starting_position();

    auto n_idx = 31;

    brd.wn() |= MSB64 >> n_idx;
    brd.white() |= MSB64 >> n_idx;
    brd.occup() |= MSB64 >> n_idx;
    assert(is_board_valid_debug(brd));

    // iterate over indexes of white knights
    for (u64 wn2 = brd.wn(); wn2;) {
        auto n_idx = std::countl_zero(wn2);

        // now iterate over possible moves for that knight
        u64 atks = knight_attacks<White>(brd, n_idx);
        while (atks) {
            auto atk_idx = std::countl_zero(atks);

            // unmake move instead of copy?
            // iterate through all knight attacks at once?
            // remove some unnecessary shifts by not dealing with index?

            // auto brd_copy = brd;
            // make_move<White, Knight>(brd_copy, n_idx, atk_idx);
            auto undo = make_move_undoable<White, Knight>(brd, n_idx, atk_idx);

            assert(is_board_valid_debug(brd));

            print_board(brd);
            undo_move(brd, undo);

            assert(is_board_valid_debug(brd));

            atks &= ~(MSB64 >> atk_idx);
        }

        wn2 &= ~(MSB64 >> n_idx);
    }

    make_move<White, Knight>(brd, n_idx, n_idx - 17);

    assert(is_board_valid_debug(brd));
}
