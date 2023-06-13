#include "chess.h"
#include "util.h"
#include <bit>
#include <cassert>
#include <iostream>

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

    // so just check if behind bit is set (>>8)
    // if it is -> 1 forward
    // else -> 2 forward

    // 1->1
    // 0->2

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

// now we basically just need a way to make sure
// you can only make legal moves (do not put you in check)

void print_2x2(u8 x) {
    u8 top = (x & 0b1100) >> 2;
    u8 bottom = x & 0b11;

    std::cout << std::bitset<2>(top) << '\n';
    std::cout << std::bitset<2>(bottom) << '\n';
}

int main(int argc, char** argv) {
    // auto brd = Board::starting_position();
    // auto brd = random_board();

    // u8 x = 0b0110;

    // print_2x2(x);

    // std::cout << "=============\n";

    // print_2x2(rev_low4(x));

    u64 res = 0;
    for (auto i = 0; i < 8; i++) {
        res |= (1ull << i) << (8 * i);
        res |= (1ull << (i + 2)) << (8 * i);
    }

    std::cout << "original data:\n";
    print_bitboard(res);
    std::cout << "horizontally mirrored:\n";
    print_bitboard(mirror_horizontal(res));

    // for (auto n_idx : BitIterator(brd.wn())) {
    //     u64 atks = knight_attacks<White>(brd, n_idx);
    //     for (auto atk_idx : BitIterator(atks)) {
    //         auto undo =
    //             make_move_undoable<White, Knight>(brd, Move(n_idx, atk_idx));

    //         print_board(brd);
    //         // print_bitboard(brd.occup());
    //         // print_bitboard(std::byteswap(brd.occup()));

    //         undo_move(brd, undo);
    //     }
    // }
}
