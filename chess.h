#pragma once

#include <array>
#include <bit>
#include <bitset>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <tuple>

#if defined(__amd64__)
#include <immintrin.h>
#endif

#if defined(__aarch64__)
#include <arm_neon.h>
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

// access with [63-idx]
// static constexpr std::array<std::string_view, 64> square_to_coords = {
//     "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
//     "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
//     "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
//     "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
//     "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
//     "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
//     "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
//     "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
// };

// so index 0 is the least significant bit (h1, i.e. bottom right)
// and the board is laid out as above. but when you index it,
// things are reversed.

/* clang-format off */
static constexpr std::array<std::string_view, 64> coords_reversed = {
    "h1", "g1", "f1", "e1", "d1", "c1", "b1", "a1",
    "h2", "g2", "f2", "e2", "d2", "c2", "b2", "a2",
    "h3", "g3", "f3", "e3", "d3", "c3", "b3", "a3",
    "h4", "g4", "f4", "e4", "d4", "c4", "b4", "a4",
    "h5", "g5", "f5", "e5", "d5", "c5", "b5", "a5",
    "h6", "g6", "f6", "e6", "d6", "c6", "b6", "a6",
    "h7", "g7", "f7", "e7", "d7", "c7", "b7", "a7",
    "h8", "g8", "f8", "e8", "d8", "c8", "b8", "a8",
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

class BitIterator {
  private:
    u64 value;

  public:
    constexpr BitIterator(u64 val) : value(val) {}

    constexpr BitIterator& operator++() {
        this->value &= this->value - 1;
        return *this;
    }

    constexpr bool operator!=(const BitIterator& other) const {
        return this->value != other.value;
    }

    constexpr u32 operator*() const { return std::countr_zero(this->value); }

    BitIterator begin() const { return *this; }
    BitIterator end() const { return BitIterator(0); }
};

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

using Mailbox = std::array<Square, 64>;

enum PieceType : u8 { Pawn, Knight, Rook, Bishop, Queen, King };

constexpr bool is_white(const PieceColor pc) { return pc == PieceColor::White; }
constexpr bool is_black(const PieceColor pc) { return pc == PieceColor::Black; }

constexpr bool is_pawn(const Square sqr) {
    return (sqr == Square::WhitePawn) || (sqr == Square::BlackPawn);
}

static constexpr std::array<char, 13> CHAR_PIECE_LOOKUP{
    'P', 'N', 'R', 'B', 'Q', 'K', 'p', 'n', 'r', 'b', 'q', 'k', '.'};

// inefficient :( but hard to make cross platform more efficiently because of
// platforms not necessarily being utf-8.

// Colors might look inverted on a dark theme
// static constexpr std::array<std::string_view, 12> UNICODE_PIECE_LOOKUP{
//     "\u2659", "\u2658", "\u2656", "\u2657", "\u2655", "\u2654",
//     "\u265F", "\u265E", "\u265C", "\u265D", "\u265B", "\u265A"};

struct Board {
    // wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk, white, black, occupancy
    // (white and black)
    u64 bitboards[12 + 3];

  public:
    // returns new board of standard starting chess position
    static constexpr Board starting_position();

    constexpr u64& wp() & noexcept { return bitboards[0]; }
    constexpr u64& wn() & noexcept { return bitboards[1]; }
    constexpr u64& wr() & noexcept { return bitboards[2]; }
    constexpr u64& wb() & noexcept { return bitboards[3]; }
    constexpr u64& wq() & noexcept { return bitboards[4]; }
    constexpr u64& wk() & noexcept { return bitboards[5]; }
    constexpr u64& bp() & noexcept { return bitboards[6]; }
    constexpr u64& bn() & noexcept { return bitboards[7]; }
    constexpr u64& br() & noexcept { return bitboards[8]; }
    constexpr u64& bb() & noexcept { return bitboards[9]; }
    constexpr u64& bq() & noexcept { return bitboards[10]; }
    constexpr u64& bk() & noexcept { return bitboards[11]; }
    constexpr u64& white() & noexcept { return bitboards[12]; }
    constexpr u64& black() & noexcept { return bitboards[13]; }
    constexpr u64& occup() & noexcept { return bitboards[14]; }

    constexpr const u64 wp() const& noexcept { return bitboards[0]; }
    constexpr const u64 wn() const& noexcept { return bitboards[1]; }
    constexpr const u64 wr() const& noexcept { return bitboards[2]; }
    constexpr const u64 wb() const& noexcept { return bitboards[3]; }
    constexpr const u64 wq() const& noexcept { return bitboards[4]; }
    constexpr const u64 wk() const& noexcept { return bitboards[5]; }
    constexpr const u64 bp() const& noexcept { return bitboards[6]; }
    constexpr const u64 bn() const& noexcept { return bitboards[7]; }
    constexpr const u64 br() const& noexcept { return bitboards[8]; }
    constexpr const u64 bb() const& noexcept { return bitboards[9]; }
    constexpr const u64 bq() const& noexcept { return bitboards[10]; }
    constexpr const u64 bk() const& noexcept { return bitboards[11]; }
    constexpr const u64 white() const& noexcept { return bitboards[12]; }
    constexpr const u64 black() const& noexcept { return bitboards[13]; }
    constexpr const u64 occup() const& noexcept { return bitboards[14]; }

    // TODO make these not templates and runtime arguments instead,
    // they should be optimized away anyway (hopefully).

    template <PieceColor c, PieceType t> constexpr u64& board() & noexcept {
        return bitboards[c * 6 + t];
    }
    template <PieceColor c> constexpr u64& color() & noexcept {
        return bitboards[12 + c];
    }
    template <PieceColor c> constexpr u64& pawns() & noexcept {
        return bitboards[c * 6];
    }
    template <PieceColor c> constexpr u64& knights() & noexcept {
        return bitboards[c * 6 + 1];
    }
    template <PieceColor c> constexpr u64& rooks() & noexcept {
        return bitboards[c * 6 + 2];
    }
    template <PieceColor c> constexpr u64& bishops() & noexcept {
        return bitboards[c * 6 + 3];
    }
    template <PieceColor c> constexpr u64& queens() & noexcept {
        return bitboards[c * 6 + 4];
    }
    template <PieceColor c> constexpr u64& king() & noexcept {
        return bitboards[c * 6 + 5];
    }

    // const versions
    template <PieceColor c, PieceType t>
    constexpr const u64 board() const& noexcept {
        return bitboards[c * 6 + t];
    }
    template <PieceColor c> constexpr const u64 color() const& noexcept {
        return bitboards[12 + c];
    }
    template <PieceColor c> constexpr const u64 pawns() const& noexcept {
        return bitboards[c * 6];
    }
    template <PieceColor c> constexpr const u64 knights() const& noexcept {
        return bitboards[c * 6 + 1];
    }
    template <PieceColor c> constexpr const u64 rooks() const& noexcept {
        return bitboards[c * 6 + 2];
    }
    template <PieceColor c> constexpr const u64 bishops() const& noexcept {
        return bitboards[c * 6 + 3];
    }
    template <PieceColor c> constexpr const u64 queens() const& noexcept {
        return bitboards[c * 6 + 4];
    }
    template <PieceColor c> constexpr const u64 king() const& noexcept {
        return bitboards[c * 6 + 5];
    }

    constexpr Board(u64 wp, u64 wn, u64 wr, u64 wb, u64 wq, u64 wk, u64 bp,
                    u64 bn, u64 br, u64 bb, u64 bq, u64 bk) noexcept {
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

    _ForceInline constexpr bool operator==(const Board& other) const noexcept {
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
    // from index
    u16 from : 6;
    // to index
    u16 to : 6;
    u16 padding_bits : 4;

    constexpr Move(u16 from, u16 to) : from(from), to(to) {}
};

constexpr u64 mirror_horizontal(u64 x) {
    // basically works by breaking down each byte into
    // groups of 2, 4, 8 and then using XOR to swap
    // certain bits to generate a permutation.
    constexpr u64 k4 = broadcast_byte(0b0000'1111);
    constexpr u64 k2 = broadcast_byte(0b0011'0011);
    constexpr u64 k1 = broadcast_byte(0b0101'0101);

    // I drew this crap out on paper for 4x4 and got the idea.
    // Same idea works for any NxN matrix where N is a power of 2 (so
    // recursion step works). Mathematical induction woohoo.
    // You will just have to do log2(N) number of steps instead.

    // remember, a^a^b = b because a^a=0 and a^0=a
    // and this is doing x=x^x^(0 or bit to swap)
    // which basically just means that it's selectively
    // swapping bits to generate a new permutation.
    x ^= k4 & (x ^ std::rotl(x, 8));
    x ^= k2 & (x ^ std::rotl(x, 4));
    x ^= k1 & (x ^ std::rotl(x, 2));
    return std::rotr(x, 7);
}

static constexpr std::string_view board_start = "8  . . . . . . . .\n"
                                                "7  . . . . . . . .\n"
                                                "6  . . . . . . . .\n"
                                                "5  . . . . . . . .\n"
                                                "4  . . . . . . . .\n"
                                                "3  . . . . . . . .\n"
                                                "2  . . . . . . . .\n"
                                                "1  . . . . . . . .\n"
                                                "\n"
                                                "   a b c d e f g h\n\n";

_OptSize _NoInline void print_board(const Board& brd) {
    Mailbox box;
    static_assert(sizeof(Square) == 1);
    static_assert(sizeof(box) == 64);
    memset(box.data(), Square::Empty, box.size());

    for (size_t i = 0; i < 12; i++) {
        for (auto tzcnt : BitIterator(brd.bitboards[i])) {
            box[63 ^ tzcnt] = static_cast<Square>(i);
        }
    }

    constexpr size_t ROW_SIZE = 8 * 2 + 3;

    std::array<char, board_start.size()> ostr;
    memcpy(ostr.data(), board_start.data(), board_start.size());

#if defined(__aarch64__)
    const auto lut = vld1q_u8((const unsigned char*)CHAR_PIECE_LOOKUP.data());
    static constexpr std::array<char, 16> row_data = {
        ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\n',
        ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\n',
    };
    auto row = vld1q_u8((const unsigned char*)row_data.data());

    size_t idx = 0;
    for (size_t i = 0; i < 4; i++) {
        auto indexes = vld1q_u8((const unsigned char*)box.data() + i * 16);
        auto chars = vqtbl1q_u8(lut, indexes);
        auto formatted = vzip1q_u8(chars, row);
        auto formatted2 = vzip2q_u8(chars, row);
        vst1q_u8((unsigned char*)ostr.data() + 3 + idx, formatted);
        vst1q_u8((unsigned char*)ostr.data() + 3 + idx + ROW_SIZE, formatted2);
        idx += 2 * ROW_SIZE;
    }
#else
    for (size_t rank = 0; rank < 8; rank++) {
        for (size_t file = 0; file < 8; file++) {
            auto sqr = box[rank * 8 + file];
            ostr[3 + ROW_SIZE * rank + 2 * file] = CHAR_PIECE_LOOKUP[sqr];
        }
    }
#endif

    static_assert(sizeof(char) == 1);
    std::cout.write(ostr.data(), ostr.size());
}

_OptSize _NoInline void print_bitboard(u64 bitboard) {
    constexpr size_t ROW_SIZE = 8 * 2 + 3;

    std::array<char, board_start.size()> ostr;
    memcpy(ostr.data(), board_start.data(), board_start.size());

    for (auto tzcnt : BitIterator(bitboard)) {
        // 63-x = x^63 because x+(63-x)=63
        // since 63 is 2^6-1, x and (63-x) occupy
        // distinct bits, the bitwise complement
        // is 63-x. this saves a register.
        u32 idx = 63 ^ tzcnt;
        size_t x_idx = idx % 8;
        size_t y_idx = idx / 8;
        ostr[3 + y_idx * ROW_SIZE + 2 * x_idx] = 'X';
    }

    std::cout.write(ostr.data(), ostr.size());
}

static inline consteval bool inbounds(auto x, auto y) {
    return ((x >= 0 && x <= 7) && (y >= 0 && y <= 7));
}

// simple but slow implementation, only used to build table at compile-time
static consteval u64 knight_attack_map(u8 sqr_idx) {
    u64 attack_map = 0;

    constexpr std::tuple<int, int> knight_offsets[8] = {
        {1, 2}, {2, 1}, {-1, 2}, {-2, 1}, {1, -2}, {2, -1}, {-1, -2}, {-2, -1},
    };

    const u32 x_idx = sqr_idx % 8;
    const u32 y_idx = sqr_idx / 8;

    for (auto [dx, dy] : knight_offsets) {
        const int new_x = x_idx + dx;
        const int new_y = y_idx + dy;

        if (inbounds(new_x, new_y)) {
            u64 sqr = 1ull << (8 * new_y + new_x);
            attack_map |= sqr;
        }
    }

    return attack_map;
}

consteval auto build_knight_table() {
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
_ForceInline constexpr u64 knight_attacks(const Board& brd, u8 sqr_idx) {
    return KNIGHT_ATTACK_TABLE[sqr_idx] & ~brd.color<c>();
}

// TODO add in checking of white/black/occup
// TODO fix const on this
constexpr bool is_board_valid_debug(const Board& brd) {
    bool is_valid = true;

    for (u32 i = 0; i < 64; i++) {
        size_t count = 0;

        Square dupes[12];

        for (size_t j = 0; j < 12; j++) {
            // check if ith bit is set starting from left (msb)
            // msb = index 0, and so on
            if ((brd.bitboards[j] >> i) & 1) {
                dupes[count++] = static_cast<Square>(j);
            }
        }

        if (count > 1) {
            is_valid = false;

            std::cout << "Square index " << i << " contains duplicates: ";
            for (size_t k = 0; k < count; k++) {
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

constexpr u64 knight_attacks_multiple(u64 knights) {
    u64 l1 = (knights >> 1) & 0x7f7f7f7f7f7f7f7full;
    u64 l2 = (knights >> 2) & 0x3f3f3f3f3f3f3f3full;
    u64 r1 = (knights << 1) & 0xfefefefefefefefeull;
    u64 r2 = (knights << 2) & 0xfcfcfcfcfcfcfcfcull;
    u64 h1 = l1 | r1;
    u64 h2 = l2 | r2;
    return (h1 << 16) | (h1 >> 16) | (h2 << 8) | (h2 >> 8);
}

template <bool h = true, bool v = true>
constexpr u64 rook_attack_map(u8 sqr_idx) {
    // TODO make some kind of macro or function or something for this?
    const int x_idx = sqr_idx % 8;
    const int y_idx = sqr_idx / 8;

    constexpr u64 file = broadcast_byte(1);

    u64 result = 0;
    if (h) {
        result |= (RANK1 << (8 * y_idx));
    }
    if (v) {
        result |= (file << x_idx);
    }
    return result;
}

inline u64 rook_attack_trick_vertical(u8 sqr_idx, u64 o) {
    // slider
    u64 s = 1ull << sqr_idx;

    auto rev = [](u64 x) { return std::byteswap(x); };

    auto m = rook_attack_map<false, true>(sqr_idx);
    o &= m;

    // u64 left = o ^ (o - 2 * s);
    u64 line_attacks = (o - 2 * s) ^ rev(rev(o) - 2 * rev(s));

    return line_attacks & m;
}

inline u64 rook_attack_trick(u8 sqr_idx, u64 o) {
    u64 result_v = rook_attack_trick_vertical(sqr_idx, o);
    // slider
    u64 s = 1ull << sqr_idx;

    auto rev = mirror_horizontal;
    auto m = rook_attack_map<true, false>(sqr_idx);
    o &= m;

    // horizontal parts only I guess?
    u64 line_attacks = (o - 2 * s) ^ rev(rev(o) - 2 * rev(s));

    return (line_attacks & m) | result_v;
}

template <PieceColor c> inline u64 rook_attacks(Board& board, u8 sqr_idx) {
    auto result = rook_attack_trick(sqr_idx, board.occup());
    // prevent self capture
    result &= ~board.color<c>();
    return result;
}

template <bool is_lr> consteval u64 generate_bishop_attack_map(u8 sqr_idx) {
    const int x_idx = sqr_idx % 8;
    const int y_idx = sqr_idx / 8;

    constexpr std::array<std::pair<int, int>, 2> dirs_lr = {{{1, 1}, {-1, -1}}};
    constexpr std::array<std::pair<int, int>, 2> dirs_rl = {{{1, -1}, {-1, 1}}};

    u64 map = 0;

    auto dirs = is_lr ? &dirs_lr : &dirs_rl;

    for (const auto& [xd, yd] : *dirs) {
        int x = x_idx;
        int y = y_idx;
        int i = 1;
        while (inbounds(x + i * xd, y + i * yd)) {
            map |= 1ull << (8 * (y + i * yd) + x + i * xd);

            i++;
        }
    }

    return map;
}

template <bool is_lr> consteval std::array<u64, 64> generate_bishop_map() {
    std::array<u64, 64> ret;
    for (int i = 0; i < 64; i++) {
        ret[i] = generate_bishop_attack_map<is_lr>(i);
    }
    return ret;
}

// bottom right = (0,0)

constexpr auto bishop_map_lr = generate_bishop_map<true>();
constexpr auto bishop_map_rl = generate_bishop_map<false>();

// lr = left to right
template <bool is_lr> constexpr u64 bishop_attack_map(u8 sqr_idx) {
    auto lut = is_lr ? bishop_map_lr.data() : bishop_map_rl.data();
    return lut[sqr_idx];
}

template <bool is_lr> inline u64 bishop_attacks_dir(u8 sqr_idx, u64 o) {
    u64 s = 1ull << sqr_idx;

    auto rev = [](u64 x) { return std::byteswap(x); };

    auto m = bishop_attack_map<is_lr>(sqr_idx);
    o &= m;

    u64 line_attacks = (o - 2 * s) ^ rev(rev(o) - 2 * rev(s));

    return line_attacks & m;
}

// MAIN FUNCTION FOR BISHOP STUFF
template <PieceColor c> u64 bishop_attacks(Board& board, u8 sqr_idx) {
    auto lr = bishop_attacks_dir<true>(sqr_idx, board.occup());
    auto rl = bishop_attacks_dir<false>(sqr_idx, board.occup());

    return (lr ^ rl) & ~board.color<c>();
}

template <PieceColor c> inline u64 queen_attacks(Board& board, u8 sqr_idx) {
    auto rook = rook_attack_trick(sqr_idx, board.occup());
    auto lr = bishop_attacks_dir<true>(sqr_idx, board.occup());
    auto rl = bishop_attacks_dir<false>(sqr_idx, board.occup());
    auto bishop = lr ^ rl;

    return (rook | bishop) & ~board.color<c>();
}

void print_bits(auto x, bool print_32 = false) {
    if (print_32) {
        std::cout << std::bitset<32>(x) << '\n';
    } else {
        std::cout << std::bitset<8>(x) << '\n';
    }
}

// double check, mostly by giving check + discovered check
//      - only possible response is king move

// but technically possible to do via ep capture
//      - basically you give 2 discovered checks at once

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
    auto ext8 = [](u64 val) -> u64 {
        u64 res = 0;
        for (u32 i = 0; i < 8; i++) {
            res |= ((val >> (8 * i)) & 1) << i;
        }
        return res;
    };

#if defined(__amd64__)
    if (!std::is_constant_evaluated()) {
        return _pext_u64(x, broadcast_byte(1 << 7));
    } else {
        return ext8(x);
    }
#else
    return ext8(x);
#endif
}

// TODO same as above
constexpr u64 dep8bits(u64 x) {
    auto dep8 = [](u64 val) -> u64 {
        u64 res = 0;
        for (auto i = 0; i < 8; i++) {
            res |= ((val >> i) & 1) << (8 * i);
        }
        return res;
    };

#if defined(__amd64__)
    if (!std::is_constant_evaluated()) {
        return _pdep_u64(x, broadcast_byte(1 << 7));
    } else {
        return dep8(x);
    }
#else
    return dep8(x);
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
template <PieceColor c, bool FWD_ONLY = false>
constexpr u64 pawn_attacks(const Board& brd) {
    u64 your_pawns = brd.pawns<c>();
    u64 occup = brd.occup();
    u64 enemy = brd.color<!c>();

    // TODO add sideway attacks
    const u64 fwd_mvs_white = (your_pawns << 8) | ((your_pawns & RANK2) << 16);
    const u64 fwd_mvs_black = (your_pawns >> 8) | ((your_pawns & RANK7) >> 16);

    const u64 side_atks_white = ((your_pawns << 9) & broadcast_byte(1)) |
                                ((your_pawns << 7) & broadcast_byte(1 << 7));
    const u64 side_atks_black = ((your_pawns >> 7) & broadcast_byte(1)) |
                                ((your_pawns >> 9) & broadcast_byte(1 << 7));

    auto fwd_mvs = is_white(c) ? fwd_mvs_white : fwd_mvs_black;
    auto side_atks = is_white(c) ? side_atks_white : side_atks_black;

    if constexpr (FWD_ONLY) {
        return (fwd_mvs & ~occup);
    } else {
        return (fwd_mvs & ~occup) | (side_atks & enemy);
    }
}

constexpr u64 rook_attacks_fixed(u64 occup, u8 sqr_idx) {
    const u8 x_idx = sqr_idx % 8;
    const u8 y_idx = sqr_idx / 8;

    const u8 shift = 8 * (7 - y_idx);
    return ((u64)(fix_bits_rank(occup >> shift, x_idx)) << shift) ^
           fix_bits_file(occup, sqr_idx);
}

// Tag that goes along move
// c = color of move made
template <PieceColor c> struct UndoTag {
    // from perspective of move doer; i.e., move to->from when undoing
    u32 from : 6;
    u32 to : 6;

    // ceil(log(6, 2))
    u32 piece_type : 3;

    // boolean flag, was there a capture
    u32 capture : 1;
    u32 capture_piece_type : 3;
};

template <PieceColor c, PieceType t>
constexpr UndoTag<c> make_move_undoable(Board& brd, Move mv) {
    // TODO maybe add debug_asserts to check for self-capture
    constexpr PieceColor to_mv = c;
    constexpr PieceColor enemy = !to_mv;

    const u64 old_piece = 1ull << mv.from;

    // we could also xor here with old_piece (not negated, these are bits
    // are always set in a valid board), but it ends up being slightly
    // more instructions on x86 on clang for some reason.
    brd.board<c, t>() &= ~old_piece;
    brd.color<to_mv>() &= ~old_piece;
    brd.occup() &= ~old_piece;

    const u64 new_piece = 1ull << mv.to;

    brd.board<c, t>() |= new_piece;
    brd.color<to_mv>() |= new_piece;

    auto capture = brd.color<enemy>() & new_piece;

    if (capture) {
        u64 b1 = brd.pawns<enemy>() & new_piece;
        u64 b2 = std::rotr(brd.knights<enemy>() & new_piece, 1);
        u64 b3 = std::rotr(brd.rooks<enemy>() & new_piece, 2);
        u64 b4 = std::rotr(brd.bishops<enemy>() & new_piece, 3);
        u64 b5 = std::rotr(brd.queens<enemy>() & new_piece, 4);
        u64 b6 = std::rotr(brd.king<enemy>() & new_piece, 5);

        // perhaps this can be explicitly vectorized with shift+movemask
        // (extract msb)

        u64 b7 = std::rotl(b1 | b2 | b3 | b4 | b5 | b6, mv.to);

        // assuming valid board, this should contain exactly 1 bit
        __assume(b7 != 0);
        u32 capture_idx = std::countl_zero(b7);

        brd.bitboards[(!c) * 6 + capture_idx] ^= new_piece;
        brd.color<enemy>() ^= new_piece;

        return UndoTag<c>{
            .from = mv.from,
            .to = mv.to,
            .piece_type = t,
            .capture = 1,
            .capture_piece_type = capture_idx,
        };
    } else {
        // no need to update if capture, since this bit was already set by
        // old enemy piece
        brd.occup() |= new_piece;

        return UndoTag<c>{
            .from = mv.from,
            .to = mv.to,
            .piece_type = t,
            .capture = 0,
        };
    }
}

template <PieceColor c> constexpr void undo_move(Board& brd, UndoTag<c> undo) {
    u64 switcher = (1ull << undo.from) | (1ull << undo.to);
    brd.bitboards[c * 6 + undo.piece_type] ^= switcher;
    brd.bitboards[12 + c] ^= switcher;

    if (undo.capture) {
        brd.bitboards[(!c) * 6 + undo.capture_piece_type] ^= 1ull << undo.to;
        // other color
        brd.bitboards[12 + (!c)] ^= 1ull << undo.to;
        brd.occup() ^= 1ull << undo.from;
    } else {
        brd.occup() ^= switcher;
    }
}

// TODO most of the template instantiations are quite similar, is this
// really necessary?
// should check if generated code is worse when passing color and type as
// runtime arguments
template <PieceColor c, PieceType t>
constexpr void make_move(Board& brd, Move mv) {
    // TODO maybe add debug_asserts to check for self-capture
    constexpr PieceColor to_mv = c;
    constexpr PieceColor enemy = !to_mv;

    const u64 old_piece = 1ull << mv.from;

    // we could also xor here with old_piece (not negated, these are bits
    // are always set in a valid board), but it ends up being slightly
    // more instructions on x86 on clang for some reason.
    brd.board<c, t>() &= ~old_piece;
    brd.color<to_mv>() &= ~old_piece;
    brd.occup() &= ~old_piece;

    const u64 new_piece = 1ull << mv.to;

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
        // no need to update if capture, since this bit was already set by
        // old enemy piece
        brd.occup() |= new_piece;
    }
}
