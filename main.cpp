#include "chess.h"

#include <cassert>
#include <iostream>

// TODO maybe this shouldn't be a template to avoid bloat
// and instead be like a function taking whatever arguments
// and use LUT to call appropriate function
// so code bloat is reduced a lot

// callback is passed the brd
template <PieceColor c, class CallbackFunc>
void iterate_pawn_moves(Board& brd, CallbackFunc callback) {
    // first iterate over forward2
    // TODO bit iterator can handle 0 properly right?

    constexpr int pawn_fwd1 = is_white(c) ? -8 : 8;

    // TODO: is it worth checking if bitboard for that is 0 already, and
    // skip the loop? Probably not I guess.

    // TODO deduplicate code?

    for (auto to_idx : BitIterator(pawn_forward1_attacks<c>(brd))) {

        // now we need to encode the move
        // TODO maybe faster to encode move by updating value
        // instead of reconstructing Move.
        // Check assembly on godbolt.
        Move mv(static_cast<int>(to_idx) + pawn_fwd1, to_idx);
        auto undo_tag = make_move_undoable<c, Pawn>(brd, mv);

        callback(brd);
        // SEGFAULT is happening on this line...
        undo_move(brd, undo_tag);
    }

    // TODO does order of this affect performance in some way?
    // if it makes a big difference then that could be explored further.
    for (auto to_idx : BitIterator(pawn_forward2_attacks<c>(brd))) {
        Move mv(static_cast<int>(to_idx) + 2 * pawn_fwd1, to_idx);
        auto undo_tag = make_move_undoable<c, Pawn>(brd, mv);

        callback(brd);
        undo_move(brd, undo_tag);
    }
}

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

// so I think for the types of moves where we can just
// generate a bitboard and use that, that's fine
// we just have separate functions for each type of
// attack

// can still do like templated function but just
// have ifs for more specific cases or something idk
// So we have everything covered for basically like
// knight, rook, bishop, queen
// I guess king moves can also be done like in the same way
// but pawn moves maybe not.

// way this works, iterate over pieces of knight type,
// then get bitboard for attacks for that piece and then
// iterate over that

// pawns can probably be done a little bit better tho.
// Instead of by piece we can do by movement type.
// We could get the bitboard for 2 forward, 1 forward,
// and infer the piece that the move applies from context.
// Then for attacks, E.P.
// let's see...

// well it's certainly possible for 2 pawns to both attack
// the same piece, but in such cases the directions will
// always be opposite.

// So we could separate the pawn attacks by direction

// then for E.P., well..
// the question for that one is can 2 pawns EP to the same
// square?
// I think only 1 possbile en passant move can exist
// for a particular turn given the rule of it just
// had to move... So this can just be handled as a
// separate step.

template <PieceType pt, PieceColor c, auto MoveGenerator, class CallbackFunc>
void iterate_moves(Board& brd, CallbackFunc callback) {
    for (auto n_idx : BitIterator(brd.board<c, pt>())) {
        u64 atks = MoveGenerator(brd, n_idx);
        for (auto atk_idx : BitIterator(atks)) {
            auto undo = make_move_undoable<c, pt>(brd, Move(n_idx, atk_idx));

            callback(brd);

            undo_move(brd, undo);
        }
    }
}

// need to add some code to do FEN string parse/dump

u64 perft_id = 0;

template <PieceColor c> u64 Perft(Board& brd, u32 depth) {

    u64 nodes = 0;

    assert(depth >= 0);

    u64 id = perft_id++;

    std::cout << "[START] Perft (id: " << id << "): called with depth " << depth
              << '\n';
    std::cout << PCOL_STR[c] << '\n';
    std::cout << "==============================\n";

    if (depth == 0)
        return 1ULL;

    // int n_moves = GenerateLegalMoves(move_list);
    // for (int i = 0; i < n_moves; i++) {
    //     MakeMove(move_list[i]);
    //     nodes += Perft(depth - 1);
    //     UndoMove(move_list[i]);
    // }

    // deduplicate?

    // // Board & needs to be called with reference, not copy!!
    // // Serious issue here!! Otherwise UB happens!
    iterate_pawn_moves<c>(brd, [depth, &nodes](Board& brd) {
        nodes += Perft<!c>(brd, depth - 1);
    });

    // (board, starting piece index)
    iterate_moves<Knight, c,
                  [](Board& brd, u32 piece_idx /* starting piece index*/) {
                      return knight_attacks<c>(brd, piece_idx);
                  }>(brd, [depth, &nodes](Board& brd) {
        nodes += Perft<!c>(brd, depth - 1);
    });

    // TODO deduplicate this code?
    iterate_moves<Bishop, c,
                  [](Board& brd, u32 piece_idx /* starting piece index*/) {
                      return bishop_attacks<c>(brd, piece_idx);
                  }>(brd, [depth, &nodes](Board& brd) {
        nodes += Perft<!c>(brd, depth - 1);
    });

    iterate_moves<Rook, c,
                  [](Board& brd, u32 piece_idx /* starting piece index*/) {
                      return rook_attacks<c>(brd, piece_idx);
                  }>(brd, [depth, &nodes](Board& brd) {
        nodes += Perft<!c>(brd, depth - 1);
    });

    iterate_moves<Queen, c,
                  [](Board& brd, u32 piece_idx /* starting piece index*/) {
                      return queen_attacks<c>(brd, piece_idx);
                  }>(brd, [depth, &nodes](Board& brd) {
        nodes += Perft<!c>(brd, depth - 1);
    });

    // king moves

    std::cout << "[END] Perft(" << depth << ") = " << nodes
              << " nodes (id: " << id << ")\n";

    return nodes;
}

int main(int argc, char** argv) {
    auto brd = Board::starting_position();

    assert(is_board_valid_debug(brd));

    // ok it actually seems correct now...

    // should probably write some code to parse
    // and generate FEN codes

    int depth = 3;
    std::cout << "Perft(" << depth << ") = " << Perft<White>(brd, depth)
              << '\n';

    std::cout << "Finished calling Perft.\n";

    // auto brd2 = parse_fen_string(
    //     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");

    // assert(is_board_valid_debug(brd2));
    // if (is_board_valid_debug(brd2)) {
    //     std::cout << "Board is valid! :)\n";
    // }

    // print_board(brd2);

    // ========================================================

    // auto brd = random_board();

    // u64 new_wb = 1ull << 30;
    // brd.wb() |= new_wb;
    // brd.white() |= new_wb;
    // brd.occup() |= new_wb;

    // u64 bq = 1ull << (30 - 4);
    // brd.bq() |= bq;
    // brd.black() |= bq;
    // brd.occup() |= bq;

    // print_board(brd);
    // // print_bitboard(rook_attack_map(30));
    // // wr in middle of board index
    // // print_bitboard(rook_attack_fix2(30, brd.occup()));
    // // print_bitboard(rook_attacks<White>(30, brd.occup()));
    // // print_bitboard(rook_attacks<White>(brd, 30));
    // // print_bitboard(bishop_attack_map<false>(30));
    // // print_bitboard(bishop_attacks<White>(brd, 30));
    // print_bitboard(queen_attacks<White>(brd, 30));

    // goal: iterate over valid game states
    // and be able to undo any moves
}
