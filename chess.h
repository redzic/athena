#include <algorithm>
#include <array>
#include <bit>
#include <bitset>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <tuple>

// TODO fix this since it doesn't actually do what it's supposed to
// probably need another header of defines or something
#if defined(__X86_64__)
#include <immintrin.h>
#endif

#define _ForceInline __attribute__((always_inline)) inline
#define _NoInline __attribute__((noinline))

// clang does not support attribute assume :(
#if defined(__clang__)
#define __assume(cond) __builtin_assume(cond);
#define _OptSize [[clang::minsize]]
#elif defined(__GNUC__)
#define __assume(cond)                                                         \
    do {                                                                       \
        if (!(cond))                                                           \
            __builtin_unreachable();                                           \
    } while (0)
#define _OptSize __attribute__((optimize("Os")))
#endif

// TODO check best practice with naming conventions on these defines

/* clang-format off */
static constexpr std::array<std::string_view, 64> square_to_coords = {
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
    "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
    "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
    "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
};
/* clang-format on */

using u64 = std::uint64_t;
using u32 = std::uint32_t;
using u16 = std::uint16_t;
using u8 = std::uint8_t;

using i64 = std::int64_t;
using i32 = std::int32_t;
using i16 = std::int16_t;
using i8 = std::int8_t;

constexpr u64 RANK1 = 0xff;
constexpr u64 RANK2 = RANK1 << 8;
constexpr u64 RANK3 = RANK2 << 8;
constexpr u64 RANK4 = RANK3 << 8;
constexpr u64 RANK5 = RANK4 << 8;
constexpr u64 RANK6 = RANK5 << 8;
constexpr u64 RANK7 = RANK6 << 8;
constexpr u64 RANK8 = RANK7 << 8;

constexpr u64 MSB64 = 1ull << 63;

consteval u64 broadcast_byte(u8 b) {
    return 0x101010101010101ull * static_cast<u64>(b);
}

enum PieceColor : u8 { White, Black };

constexpr PieceColor operator!(PieceColor orig) {
    orig = static_cast<PieceColor>(!static_cast<u8>(orig));
    return orig;
}

enum Square : u8 {
    WhitePawn,
    WhiteKnight,
    WhiteRook,
    WhiteBishop,
    WhiteQueen,
    WhiteKing,
    BlackPawn,
    BlackKnight,
    BlackRook,
    BlackBishop,
    BlackQueen,
    BlackKing,
    Empty
};

enum PieceType : u8 { Pawn, Knight, Rook, Bishop, Queen, King };

constexpr bool is_white(const PieceColor pc) { return pc == PieceColor::White; }
constexpr bool is_black(const PieceColor pc) { return pc == PieceColor::Black; }

constexpr bool is_pawn(const Square sqr) {
    return (sqr == Square::WhitePawn) || (sqr == Square::BlackPawn);
}

static constexpr std::array<char, 13> CHAR_PIECE_LOOKUP{
    'P', 'N', 'R', 'B', 'Q', 'K', 'p', 'n', 'r', 'b', 'q', 'k', ' '};

struct Board {
    // wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk, white, black, occupancy
    // (white and black)
    u64 bitboards[12 + 3];

  public:
    // returns new board of standard starting chess position
    static constexpr Board starting_position();

    // TODO somehow figure out const version

    constexpr u64& wp() { return bitboards[0]; }
    constexpr u64& wn() { return bitboards[1]; }
    constexpr u64& wr() { return bitboards[2]; }
    constexpr u64& wb() { return bitboards[3]; }
    constexpr u64& wq() { return bitboards[4]; }
    constexpr u64& wk() { return bitboards[5]; }
    constexpr u64& bp() { return bitboards[6]; }
    constexpr u64& bn() { return bitboards[7]; }
    constexpr u64& br() { return bitboards[8]; }
    constexpr u64& bb() { return bitboards[9]; }
    constexpr u64& bq() { return bitboards[10]; }
    constexpr u64& bk() { return bitboards[11]; }
    constexpr u64& white() { return bitboards[12]; }
    constexpr u64& black() { return bitboards[13]; }
    constexpr u64& occup() { return bitboards[14]; }

    template <PieceColor c, PieceType t> constexpr u64& board() {
        return bitboards[c * 6 + t];
    }

    template <PieceColor c> constexpr u64& color() { return bitboards[12 + c]; }

    template <PieceColor c> constexpr u64& pawns() { return bitboards[c * 6]; }
    template <PieceColor c> constexpr u64& knights() {
        return bitboards[c * 6 + 1];
    }
    template <PieceColor c> constexpr u64& rooks() {
        return bitboards[c * 6 + 2];
    }
    template <PieceColor c> constexpr u64& bishops() {
        return bitboards[c * 6 + 3];
    }
    template <PieceColor c> constexpr u64& queens() {
        return bitboards[c * 6 + 4];
    }
    template <PieceColor c> constexpr u64& king() {
        return bitboards[c * 6 + 5];
    }

    constexpr Board(u64 wp, u64 wn, u64 wr, u64 wb, u64 wq, u64 wk, u64 bp,
                    u64 bn, u64 br, u64 bb, u64 bq, u64 bk) {
        bitboards[0] = wp;
        bitboards[1] = wn;
        bitboards[2] = wr;
        bitboards[3] = wb;
        bitboards[4] = wq;
        bitboards[5] = wk;
        bitboards[6] = bp;
        bitboards[7] = bn;
        bitboards[8] = br;
        bitboards[9] = bb;
        bitboards[10] = bq;
        bitboards[11] = bk;
        u64 white = wp | wn | wr | wb | wq | wk;
        u64 black = bp | bn | br | bb | bq | bk;
        bitboards[12] = white;
        bitboards[13] = black;
        bitboards[14] = white | black;
    }

    _ForceInline constexpr bool operator==(const Board& other) const {
        // optimization; only compare the first 12 elements of the bitboards
        // and assume that the rest are valid.
        // TODO add debug_assert for last elements

        if (std::is_constant_evaluated()) {
            for (size_t i = 0; i < 12; i++) {
                if (this->bitboards[i] != other.bitboards[i]) {
                    return false;
                }
            }
            return true;
        } else {
            return std::memcmp(this->bitboards, other.bitboards,
                               sizeof(u64) * 12) == 0;
        }
    }
};

constexpr Board Board::starting_position() {
    constexpr u64 wp = RANK2;
    constexpr u64 bp = RANK7;
    constexpr u64 wr = 0b10000001ull;
    constexpr u64 br = 0b10000001ull << 56;
    constexpr u64 wn = 0b01000010ull;
    constexpr u64 bn = 0b01000010ull << 56;
    constexpr u64 wb = 0b00100100ull;
    constexpr u64 bb = 0b00100100ull << 56;
    constexpr u64 wq = 0b00010000ull;
    constexpr u64 bq = 0b00010000ull << 56;
    constexpr u64 wk = 0b00001000ull;
    constexpr u64 bk = 0b00001000ull << 56;

    constexpr Board b(wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk);

    return b;
}

struct Move {
    u16 from_idx : 3;
    u16 to_idx : 3;
    u16 tag_bits : 4;
};

_OptSize _NoInline void print_board(const Board& brd) {
    std::array<Square, 64> array_brd;
    static_assert(sizeof(Square) == 1);
    memset(array_brd.data(), Square::Empty, 64);

    // 8 spaces + 9 bars + 1 newline character
    constexpr auto ROW_LEN = 8 + 9 + 1;
    constexpr auto NUM_CHARS = ROW_LEN * (8 + 9);

    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+
    // | | | | | | | | |
    // +-+-+-+-+-+-+-+-+

    std::array<char, NUM_CHARS> board_str;

    static constexpr std::string_view str_row =
        "+-+-+-+-+-+-+-+-+\n| | | | | | | | |\n";

    static_assert(str_row.size() == (2 * ROW_LEN));

    for (size_t i = 0; i < 8; i++) {
        std::memcpy(board_str.data() + (i * str_row.size()), str_row.data(),
                    str_row.size());
    }
    std::memcpy(board_str.data() + (8 * str_row.size()), str_row.data(),
                ROW_LEN);

    for (size_t i = 0; i < 12; i++) {
        // TODO figure out how to move this into its own iterator
        // preferably with zero-cost abstraction
        for (auto bb = brd.bitboards[i]; bb;) {
            const auto lzcnt = std::countl_zero(bb);
            array_brd[lzcnt] = static_cast<Square>(i);
            bb &= ~(MSB64 >> lzcnt);
        }
    }

    for (size_t i = 0; i < 64; i++) {
        size_t y_idx = 1 + 2 * (i / 8);
        size_t x_idx = 1 + 2 * (i % 8);

        board_str[ROW_LEN * y_idx + x_idx] = CHAR_PIECE_LOOKUP[array_brd[i]];
    }

    std::cout << std::string_view(board_str.data(), NUM_CHARS);
}

_OptSize _NoInline void print_bitboard(u64 bitboard) {
    for (u32 i = 0; i < 8; i++) {
        for (u32 j = 0; j < 8; j++) {
            auto bit = ((bitboard << j) >> 63) & 1;
            putchar('0' + bit);
            // if (j != 7)
            //     putchar(' ');
        }

        bitboard <<= 8;

        putchar('\n');
    }
}

// simple but slow implementation, only used to build table at compile-time
consteval u64 knight_attack_map(u8 sqr_idx) {
    u64 attack_map = 0;

    constexpr std::tuple<int, int> knight_offsets[8] = {
        {1, 2}, {2, 1}, {-1, 2}, {-2, 1}, {1, -2}, {2, -1}, {-1, -2}, {-2, -1},
    };

    const auto x_idx = sqr_idx % 8;
    const auto y_idx = sqr_idx / 8;

    for (const auto& [dx, dy] : knight_offsets) {
        const int new_x = x_idx + dx;
        const int new_y = y_idx + dy;

        if ((new_x >= 0 && new_x <= 7) && (new_y >= 0 && new_y <= 7)) {
            u64 sqr = MSB64 >> (8 * new_y + new_x);
            attack_map |= sqr;
        }
    }

    return attack_map;
}

consteval std::array<u64, 64> build_knight_table() {
    std::array<u64, 64> table;

    for (size_t i = 0; i < 64; i++) {
        table[i] = knight_attack_map(i);
    }

    return table;
}

static constexpr auto KNIGHT_ATTACK_TABLE = build_knight_table();

// assume white pieces

// TODO fix const correctness... ugh...
// kinda inconvenient but whatever.
template <PieceColor c>
_ForceInline constexpr u64 knight_attacks(Board& brd, u8 sqr_idx) {
    return KNIGHT_ATTACK_TABLE[sqr_idx] & ~brd.color<c>();
}

// TODO add in checking of white/black/occup
// TODO fix const on this
constexpr bool is_board_valid_debug(Board brd) {
    auto is_valid = true;

    for (auto i = 0; i < 64; i++) {
        int count = 0;

        Square dupes[12];

        for (auto j = 0; j < 12; j++) {
            // check if ith bit is set starting from left (msb)
            // msb = index 0, and so on
            if ((brd.bitboards[j] << i) & MSB64) {
                dupes[count++] = static_cast<Square>(j);
            }
        }

        if (count > 1) {
            is_valid = false;

            std::cout << "Square index " << i << " contains duplicates: ";
            for (auto k = 0; k < count; k++) {
                std::cout << CHAR_PIECE_LOOKUP[dupes[k]];
                if (k != (count - 1)) {
                    std::cout << ", ";
                }
            }
            std::cout << '\n';
        }
    }

    if (brd.white() !=
        (brd.wp() | brd.wn() | brd.wb() | brd.wr() | brd.wq() | brd.wk())) {
        std::cout << "White bitboard is missing pieces\n";
        return false;
    }
    if (brd.black() !=
        (brd.bp() | brd.bn() | brd.bb() | brd.br() | brd.bq() | brd.bk())) {
        std::cout << "Black bitboard is missing pieces\n";

        return false;
    }
    if (brd.occup() != (brd.white() | brd.black())) {
        std::cout << "Occupancy bitboard is missing pieces\n";
        return false;
    }

    return is_valid;
}

// checks if there are multiple bitboards with the same bits set
// for any of the squares

// bro how the heck does this work...
constexpr u64 knight_attacks_multiple(u64 knights) {
    u64 l1 = (knights >> 1) & 0x7f7f7f7f7f7f7f7full;
    u64 l2 = (knights >> 2) & 0x3f3f3f3f3f3f3f3full;
    u64 r1 = (knights << 1) & 0xfefefefefefefefeull;
    u64 r2 = (knights << 2) & 0xfcfcfcfcfcfcfcfcull;
    u64 h1 = l1 | r1;
    u64 h2 = l2 | r2;
    return (h1 << 16) | (h1 >> 16) | (h2 << 8) | (h2 >> 8);
}

constexpr u64 rook_attacks(u8 sqr_idx) {
    // TODO make some kind of macro or function or something for this?
    const int x_idx = sqr_idx % 8;
    const int y_idx = sqr_idx / 8;

    constexpr u64 file = broadcast_byte((u8)1 << 7);

    return (RANK8 >> (8 * y_idx)) ^ (file >> x_idx);
}

void print_bits(auto x, bool print_32 = false) {
    if (print_32) {
        std::cout << std::bitset<32>(x) << '\n';
    } else {
        std::cout << std::bitset<8>(x) << '\n';
    }
}
// void print_bits(auto x) { std::cout << std::bitset<8>(x) << '\n'; }

// can be done with blsr instruction x86
// aka x &= x - 1
// but will iterate over lsbs first
// and wont give you index lol

// TODO perhaps we can just accept a templated callback argument?
// that receives each index
constexpr void bit_loop(u64 bits) {
    while (bits) {
        const auto lzcnt = std::countl_zero(bits);

        // use lzcnt (index) here
        std::cout << lzcnt << '\n';

        bits &= ~(MSB64 >> lzcnt);
    }
}

constexpr u8 fix_bits_rank(u8 occup, u8 idx) {
    occup &= ~((1 << 7) >> idx);

    const u16 bit1 = (1 << 7) >> idx;

    u8 left = occup >> (7 - idx);
    u8 right = occup << idx;

    u8 lz = std::countr_zero(left);
    u8 rz = std::countl_zero(right);

    u16 ls = (bit1 << lz);
    ls |= ls - 1;

    u16 rs = (bit1 >> rz);
    rs = rs == 0 ? 0xff : ~(rs - 1);

    u16 result = ls & rs;

    return (u8)(result & 0xff);
}

// TODO optimize functions that call this for for no pext
constexpr u64 ext8bits(u64 x) {
#if defined(__X86_64__)
    return _pext_u64(x, broadcast_byte(1 << 7));
#else
    u64 res = 0;
    for (auto i = 0; i < 8; i++) {
        res |= ((x >> (8 * i)) & 1) << i;
    }
    return res;
#endif
}

// TODO same as above
constexpr u64 dep8bits(u64 x) {
#if defined(__X86_64__)
    return _pdep_u64(x, broadcast_byte(1 << 7));
#else
    u64 res = 0;
    for (auto i = 0; i < 8; i++) {
        res |= ((x >> i) & 1) << (8 * i);
    }
    return res;
#endif
}

// row_idx is 0-7
constexpr u64 fix_bits_file(u64 occup, u8 sqr_idx) {
    const u8 x_idx = sqr_idx % 8;
    const u8 y_idx = sqr_idx / 8;

    u64 occup_file1 = (occup << x_idx) & broadcast_byte(1 << 7);
    u8 file1 = (u8)(ext8bits(occup_file1) & 0xff);

    return dep8bits(fix_bits_rank(file1, y_idx)) >> x_idx;
}

// TODO does C++ ensure l/r shift shifts into zeros?

// TODO just template over piece color
// TODO make sure everything is optimized out properly
template <PieceColor c>
constexpr u64 pawns_atk(u64 your_pawns, u64 occup, u64 enemy) {
    // TODO add sideway attacks
    const u64 fwd_mvs_white = (your_pawns << 8) | ((your_pawns & RANK2) << 16);
    const u64 fwd_mvs_black = (your_pawns >> 8) | ((your_pawns & RANK7) >> 16);

    const u64 side_atks_white = ((your_pawns << 9) & broadcast_byte(1)) |
                                ((your_pawns << 7) & broadcast_byte(1 << 7));
    const u64 side_atks_black = ((your_pawns >> 7) & broadcast_byte(1)) |
                                ((your_pawns >> 9) & broadcast_byte(1 << 7));

    auto fwd_mvs = is_white(c) ? fwd_mvs_white : fwd_mvs_black;
    auto side_atks = is_white(c) ? side_atks_white : side_atks_black;

    return (fwd_mvs & ~occup) | (side_atks & enemy);
}

constexpr u64 rook_attacks_fixed(u64 occup, u8 sqr_idx) {
    const u8 x_idx = sqr_idx % 8;
    const u8 y_idx = sqr_idx / 8;

    const u8 shift = 8 * (7 - y_idx);
    return ((u64)(fix_bits_rank(occup >> shift, x_idx)) << shift) ^
           fix_bits_file(occup, sqr_idx);
}

// TODO remove _idx from obvious stuff

// Tag that goes along move
// c = color of move made
template <PieceColor c> struct UndoTag {
    // from perspective of move undoer; i.e., move from->to when undoing
    u32 from : 3;
    u32 to : 3;

    // ceil(log(6, 2))
    // technically not needed but saves us from redoing the same work
    // to figure out what the piece type of the moved piece was
    u32 piece_type : 3;

    // boolean flag, was there a capture
    u32 capture : 1;
    u32 capture_piece_type : 3;
    u32 capture_idx : 3;
};

template <PieceColor c, PieceType t>
constexpr UndoTag<c> make_move_undoable(Board& brd, u8 from_idx, u8 to_idx) {
    // TODO maybe add debug_asserts to check for self-capture
    constexpr PieceColor to_mv = c;
    constexpr PieceColor enemy = !to_mv;

    const u64 old_piece = MSB64 >> from_idx;

    // we could also xor here with old_piece (not negated, these are bits
    // are always set in a valid board), but it ends up being slightly
    // more instructions on x86 on clang for some reason.
    brd.board<c, t>() &= ~old_piece;
    brd.color<to_mv>() &= ~old_piece;
    brd.occup() &= ~old_piece;

    const u64 new_piece = MSB64 >> to_idx;

    brd.board<c, t>() |= new_piece;
    brd.color<to_mv>() |= new_piece;

    auto capture = brd.color<enemy>() & new_piece;

    if (capture) {
        auto b1 = brd.pawns<enemy>() & new_piece;
        auto b2 = std::rotr(brd.knights<enemy>() & new_piece, 1);
        auto b3 = std::rotr(brd.rooks<enemy>() & new_piece, 2);
        auto b4 = std::rotr(brd.bishops<enemy>() & new_piece, 3);
        auto b5 = std::rotr(brd.queens<enemy>() & new_piece, 4);
        auto b6 = std::rotr(brd.king<enemy>() & new_piece, 5);

        // perhaps this can be explicitly vectorized with shift+movemask
        // (extract msb)

        auto b7 = std::rotl(b1 | b2 | b3 | b4 | b5 | b6, to_idx);
        __assume(b7 != 0);
        auto capture_idx = std::countl_zero(b7);

        // could also xor? might be easier ig
        brd.bitboards[(!c) * 6 + capture_idx] ^= new_piece;
        brd.color<enemy>() ^= new_piece;

        // TODO find some kind of way for compiler to warn about
        // potentially returning nothing

        return UndoTag<c>{
            .from = from_idx,
            .to = to_idx,
            .piece_type = t,
            .capture = 1,
            .capture_piece_type = capture_idx,
            .capture_idx = to_idx,
        };
    } else {
        // no need to update if capture, since this bit was already set by old
        // enemy piece
        brd.occup() |= new_piece;

        return UndoTag<c>{
            .from = from_idx,
            .to = to_idx,
            .piece_type = t,
            .capture = 0,
        };
    }
}

// TODO most of the template instantiations are quite similar, is this
// really necessary?
// should check if generated code is worse when passing color and type as
// runtime arguments
template <PieceColor c, PieceType t>
constexpr void make_move(Board& brd, u8 from_idx, u8 to_idx) {
    // TODO maybe add debug_asserts to check for self-capture
    constexpr PieceColor to_mv = c;
    constexpr PieceColor enemy = !to_mv;

    const u64 old_piece = MSB64 >> from_idx;

    // we could also xor here with old_piece (not negated, these are bits
    // are always set in a valid board), but it ends up being slightly
    // more instructions on x86 on clang for some reason.
    brd.board<c, t>() &= ~old_piece;
    brd.color<to_mv>() &= ~old_piece;
    brd.occup() &= ~old_piece;

    const u64 new_piece = MSB64 >> to_idx;

    brd.board<c, t>() |= new_piece;
    brd.color<to_mv>() |= new_piece;

    auto capture = brd.color<enemy>() & new_piece;

    if (capture) {
        /* clang-format off */
        brd.pawns   <enemy>() &= ~new_piece;
        brd.knights <enemy>() &= ~new_piece;
        brd.rooks   <enemy>() &= ~new_piece;
        brd.bishops <enemy>() &= ~new_piece;
        brd.queens  <enemy>() &= ~new_piece;
        brd.king    <enemy>() &= ~new_piece;
        brd.color   <enemy>() &= ~new_piece;
        /* clang-format on */
    } else {
        // no need to update if capture, since this bit was already set by old
        // enemy piece
        brd.occup() |= new_piece;
    }
}
