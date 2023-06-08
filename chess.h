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

// clang does not support attribute assume :(
#if defined(__clang__)
#define _ASSUME(cond) __builtin_assume(cond);
#elif defined(__GNUC__)
#define _ASSUME(cond) [[assume(cond)]];
#endif

// TODO check best practice with naming conventions on these defines

#define _ForceInline __attribute__((always_inline)) inline
#define _NoInline __attribute__((noinline))

using u64 = std::uint64_t;
using u32 = std::uint32_t;
using u16 = std::uint16_t;
using u8 = std::uint8_t;

using i64 = std::int64_t;
using i32 = std::int32_t;
using i16 = std::int16_t;
using i8 = std::int8_t;

constexpr u64 RANK1 = 0b1111'1111ull;
constexpr u64 RANK2 = RANK1 << 8;
constexpr u64 RANK3 = RANK2 << 8;
constexpr u64 RANK4 = RANK3 << 8;
constexpr u64 RANK5 = RANK4 << 8;
constexpr u64 RANK6 = RANK5 << 8;
constexpr u64 RANK7 = RANK6 << 8;
constexpr u64 RANK8 = RANK7 << 8;

constexpr u64 MSB64 = (1ull << 63);

consteval u64 broadcast_byte(const u8 b) {
    return 0x101010101010101ull * static_cast<u64>(b);
}

enum PieceColor : u8 { White = 0, Black = 1 };

constexpr PieceColor operator!(PieceColor orig) {
    orig = static_cast<PieceColor>(!static_cast<u8>(orig));
    return orig;
}

enum Square : u8 { wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk, Empty };

enum PieceType : u8 { Pawn, Knight, Rook, Bishop, Queen, King };

constexpr bool is_white(const PieceColor pc) noexcept {
    return pc == PieceColor::White;
}
constexpr bool is_black(const PieceColor pc) noexcept {
    return pc == PieceColor::Black;
}

constexpr bool is_pawn(const Square sqr) noexcept {
    return (sqr == Square::wp) || (sqr == Square::bp);
}

constexpr std::array<char, 13> CHAR_PIECE_LOOKUP{
    'P', 'N', 'R', 'B', 'Q', 'K', 'p', 'n', 'r', 'b', 'q', 'k', ' '};

struct Board {
    // wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk, white, black, occupied
    // (white and black)

    // TODO might actually be a good idea to represent these
    // as individual u64, that way it ends up registers?
    // although... I feel like it probably wouldn't help/fit
    // since there's just too many bitboards to begin with
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
        return bitboards[static_cast<u8>(c) * 6 + static_cast<u8>(t)];
    }

    template <PieceColor c> constexpr u64& color() {
        return bitboards[12 + static_cast<u8>(c)];
    }

    template <PieceColor c> constexpr u64& pawns() {
        return bitboards[static_cast<u8>(c) * 6];
    }
    template <PieceColor c> constexpr u64& knights() {
        return bitboards[static_cast<u8>(c) * 6 + 1];
    }
    template <PieceColor c> constexpr u64& rooks() {
        return bitboards[static_cast<u8>(c) * 6 + 2];
    }
    template <PieceColor c> constexpr u64& bishops() {
        return bitboards[static_cast<u8>(c) * 6 + 3];
    }
    template <PieceColor c> constexpr u64& queens() {
        return bitboards[static_cast<u8>(c) * 6 + 4];
    }
    template <PieceColor c> constexpr u64& king() {
        return bitboards[static_cast<u8>(c) * 6 + 5];
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
        u64 white = (wp | wn | wr | wb | wq | wk);
        u64 black = (bp | bn | br | bb | bq | bk);
        bitboards[12] = white;
        bitboards[13] = black;
        bitboards[14] = white | black;
    }

    _ForceInline constexpr bool operator==(const Board& other) const {
        // optimization; only compare the first 12 elements of the bitboards
        // and assume that the rest are valid.
        // TODO add debug_assert for last elements

        if (std::is_constant_evaluated()) {
            for (auto i = 0; i < 12; i++) {
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
    u64 wp = ((1ull << 8) - 1) << 8;
    u64 bp = ((1ull << 8) - 1) << (56 - 8);
    u64 wr = 0b10000001ull;
    u64 br = 0b10000001ull << 56;
    u64 wn = 0b01000010ull;
    u64 bn = 0b01000010ull << 56;
    u64 wb = 0b00100100ull;
    u64 bb = 0b00100100ull << 56;
    u64 wq = 0b00010000ull;
    u64 bq = 0b00010000ull << 56;
    u64 wk = 0b00001000ull;
    u64 bk = 0b00001000ull << 56;

    Board b(wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk);

    return b;
}

constexpr u8 MASK6 = (1 << 6) - 1;

struct Move {
    u16 bits;

  public:
    constexpr u8 from_idx() { return bits & MASK6; }
    constexpr u8 to_idx() { return (bits << 6) & MASK6; }
    constexpr u8 tag_bits() { return (bits << 12); }

    constexpr Move(u8 from, u8 to, u8 tag);
};

constexpr Move::Move(u8 from, u8 to, u8 tag) {
    // it's saying this was not constructed/initialized?
    bits = (from & MASK6) | ((to & MASK6) >> 6) | ((tag & 0xf) >> 12);
}

_NoInline constexpr void print_board(const Board& brd) {
    std::array<Square, 64> array_brd;
    std::fill(array_brd.begin(), array_brd.end(), Square::Empty);

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

    constexpr std::string_view str_row =
        "+-+-+-+-+-+-+-+-+\n| | | | | | | | |\n";

    static_assert(str_row.size() == (2 * ROW_LEN));

    for (auto i = 0; i < 8; i++) {
        std::memcpy(board_str.data() + (i * str_row.size()), str_row.data(),
                    str_row.size());
    }
    std::memcpy(board_str.data() + (8 * str_row.size()), str_row.data(),
                ROW_LEN);

    for (auto i = 0; i < 12; i++) {
        // TODO figure out how to move this into its own iterator
        // preferably with zero-cost abstraction
        for (auto bb = brd.bitboards[i]; bb;) {
            const auto lzcnt = std::countl_zero(bb);
            array_brd[lzcnt] = static_cast<Square>(i);
            bb &= ~(MSB64 >> lzcnt);
        }
    }

    for (auto i = 0; i < 64; i++) {
        auto y_idx = 1 + 2 * (i / 8);
        auto x_idx = 1 + 2 * (i % 8);

        board_str[ROW_LEN * y_idx + x_idx] = CHAR_PIECE_LOOKUP[array_brd[i]];
    }

    std::cout << std::string_view(board_str.data(), NUM_CHARS);
}

_NoInline constexpr void print_bitboard(u64 bitboard) {
    for (auto i = 0; i < 8; i++) {
        for (auto j = 0; j < 8; j++) {
            auto bit = ((bitboard << j) >> 63) & 1;
            putchar('0' + bit);
            // if (j != 7)
            //     putchar(' ');
        }

        bitboard <<= 8;

        puts("");
    }
}

// simple but slow implementation, only used to build table at compile-time
consteval u64 knight_attack_map(const u8 sqr_idx) {
    u64 attack_map = 0;

    const std::tuple<int, int> knight_offsets[8] = {
        {1, 2}, {2, 1}, {-1, 2}, {-2, 1}, {1, -2}, {2, -1}, {-1, -2}, {-2, -1},
    };

    const int x_idx = sqr_idx % 8;
    const int y_idx = sqr_idx / 8;

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

    // should these be size_t? does that change codegen?
    // when things are statically known... (not constexpr just statically known
    // upper bound)
    for (auto i = 0; i < 64; i++) {
        table[i] = knight_attack_map(i);
    }

    return table;
}

constinit static const auto KNIGHT_ATTACK_TABLE = build_knight_table();

// assume white pieces

// TODO fix const correctness... ugh...
// kinda inconvenient but whatever.
template <PieceColor c>
_ForceInline constexpr u64 knight_attacks(Board& brd,
                                          const u8 sqr_idx) noexcept {
    return KNIGHT_ATTACK_TABLE[sqr_idx] & ~brd.color<c>();
}

// TODO add in checking of white/black/occup
// TODO fix const on this
constexpr bool is_board_valid_debug(Board brd) {
    auto is_valid = true;

    for (auto i = 0; i < 64; i++) {
        int count = 0;

        // yep I guess compiler already automatically reuses the allocation

        // TODO maybe make separate PieceType?
        // so that it does not have empty value...
        // or maybe that's unnecessary.
        Square dupes[12];

        for (auto j = 0; j < 12; j++) {
            // count += (brd.bitboards[j] >> (63 - i)) & 1;
            if ((brd.bitboards[j] >> (63 - i)) & 1) {
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
constexpr u64 knight_attacks_multiple(const u64 knights) {
    u64 l1 = (knights >> 1) & 0x7f7f7f7f7f7f7f7full;
    u64 l2 = (knights >> 2) & 0x3f3f3f3f3f3f3f3full;
    u64 r1 = (knights << 1) & 0xfefefefefefefefeull;
    u64 r2 = (knights << 2) & 0xfcfcfcfcfcfcfcfcull;
    u64 h1 = l1 | r1;
    u64 h2 = l2 | r2;
    return (h1 << 16) | (h1 >> 16) | (h2 << 8) | (h2 >> 8);
}

constexpr u64 rook_attacks(const u8 sqr_idx) {
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

constexpr u8 fix_bits_rank(u8 occup, const u8 idx) {
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
constexpr u64 fix_bits_file(u64 occup, const u8 sqr_idx) {
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
constexpr u64 pawns_atk(u64 your_pawns, u64 occup, u64 enemy) noexcept {
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

constexpr u64 rook_attacks_fixed(u64 occup, const u8 sqr_idx) noexcept {
    const u8 x_idx = sqr_idx % 8;
    const u8 y_idx = sqr_idx / 8;

    const u8 shift = 8 * (7 - y_idx);
    return ((u64)(fix_bits_rank(occup >> shift, x_idx)) << shift) ^
           fix_bits_file(occup, sqr_idx);
}

// TODO most of the template instantiations are quite similar, is this
// really necessary?
// should check if generated code is worse when passing color and type as
// runtime arguments
template <PieceColor c, PieceType t>
constexpr void make_move(Board& brd, const u8 from_idx, const u8 to_idx) {
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
