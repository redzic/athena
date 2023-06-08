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

constexpr u64 msb = (1ull << 63);

consteval u64 broadcast_byte(const u8 b) {
    return 0x101010101010101ull * static_cast<u64>(b);
}

enum PieceColor : u8 { White = 0, Black = 1 };

enum Square : u8 { wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk, Empty };

bool is_pawn(Square sqr) { return (sqr == Square::wp) || (sqr == Square::bp); }

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

    inline bool operator==(const Board& other) const {
        // optimization; only compare the first 12 elements of the bitboards
        // and assume that the rest are valid.
        // TODO add debug_assert for last elements
        return std::memcmp(this->bitboards, other.bitboards,
                           sizeof(u64) * 12) == 0;
        // return 0;
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

void print_board(const Board& brd) {
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

    static constexpr std::string_view str_row =
        "+-+-+-+-+-+-+-+-+\n| | | | | | | | |\n";

    static_assert(str_row.size() == (2 * ROW_LEN));

    for (auto i = 0; i < 8; i++) {
        std::memcpy(board_str.data() + (i * str_row.size()), str_row.data(),
                    str_row.size());
    }
    std::memcpy(board_str.data() + (8 * str_row.size()), str_row.data(),
                ROW_LEN);

    for (auto i = 0; i < 12; i++) {
        // do simple way for now
        // TODO figure out how to do bit loop later with lzcnt or whatever
        // for (auto j = 0; j < 64; j++) {
        //     // TODO check if this is better to do branchless (maybe not
        //     though)

        //     if (brd.bitboards[i] & (msb >> j)) {
        //         array_brd[j] = static_cast<Square>(i);
        //     }
        // }

        // TODO figure out how to move this into its own iterator
        // preferably with zero-cost abstraction
        for (auto bb = brd.bitboards[i]; bb;) {
            const auto lzcnt = std::countl_zero(bb);

            array_brd[lzcnt] = static_cast<Square>(i);

            bb &= ~(msb >> lzcnt);
        }
    }

    for (auto i = 0; i < 64; i++) {
        auto y_idx = 1 + 2 * (i / 8);
        auto x_idx = 1 + 2 * (i % 8);

        board_str[ROW_LEN * y_idx + x_idx] = CHAR_PIECE_LOOKUP[array_brd[i]];
    }

    std::cout << std::string_view(board_str.data(), NUM_CHARS);
}

void print_bitboard(u64 bitboard) {
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
            u64 sqr = ((1ull << 63) >> (8 * new_y)) >> new_x;
            attack_map |= sqr;
        }
    }

    return attack_map;
}

consteval std::array<u64, 64> build_knight_table() {
    std::array<u64, 64> table;

    for (auto i = 0; i < 64; i++) {
        table[i] = knight_attack_map(i);
    }

    return table;
}

static constexpr auto KNIGHT_ATTACK_TABLE = build_knight_table();

// assume white pieces

// TODO fix const correctness... ugh...
// kinda inconvenient but whatever.
constexpr u64 knight_attacks(Board& brd, const u8 sqr_idx) {
    // return KNIGHT_ATTACK_TABLE[sqr_idx];
    return KNIGHT_ATTACK_TABLE[sqr_idx] & (~brd.white());
}

// seems best on clang since the compiled code is branchless
constexpr u64 knight_attacks_bitwise(const Board& brd, const u8 sqr_idx) {
    // TODO figure out how to make debug assert in C++
    // debug_assert(sqr_idx <= 63);
    u64 attack_map = 0;

    // center = 2,2 = 18

    // 01010000
    // 10001000
    // 00000000
    // 10001000
    // 01010000
    // 00000000
    // 00000000
    // 00000000

    constexpr u64 N_ATTACK_BITS =
        0b01010000'10001000'00000000'10001000'01010000'00000000'00000000'00000000;

    int dist = 18 - (int)sqr_idx;

    // i.e. center > sqr_idx
    if (dist > 0) {
        attack_map = N_ATTACK_BITS << dist;
    } else {
        attack_map = N_ATTACK_BITS >> (-dist);
    }

    u8 x_idx = sqr_idx % 8;

    constexpr u64 FIX_OOB = broadcast_byte(0xf0);

    if (x_idx <= 1) {
        attack_map &= FIX_OOB;
    } else if (x_idx >= 6) {
        attack_map &= FIX_OOB >> 4;
    }

    // TODO figure out what happens when king is attacked...
    // surely that can't be a legal move right?

    // return attack_map & (~(*brd.white()));
    return attack_map;
}

// assume white pieces
constexpr u64 knight_attacks_fast(const Board& brd, const u8 sqr_idx) {
    // TODO figure out how to make debug assert in C++
    // debug_assert(sqr_idx <= 63);

    constexpr u64 DUP8_BITS = 0x101010101010101;

    constexpr u64 N_ATTACK_LEFT =
        0b00000010'00000100'00000000'00000100'00000010'00000000'00000000'00000000;
    constexpr u64 N_ATTACK_RIGHT =
        0b01000000'00100000'00000000'00100000'01000000'00000000'00000000'00000000;

    const u8 x = sqr_idx % 8;
    const u8 y = sqr_idx / 8;

    // auto r_shift = sqr_idx;
    const u64 rmask = ((u64)((u8)(((u8)RANK1) >> x))) * DUP8_BITS;

    const u64 lmask = ((u64)((u8)(((u8)RANK1) << (7 - x)))) * DUP8_BITS;

    u64 right_side = (N_ATTACK_RIGHT >> x) & rmask;
    u64 left_side = (N_ATTACK_LEFT << (7 - x)) & lmask;

    u64 attacks = left_side | right_side;

    if (y <= 1) {
        return attacks << (8 * (2 - y));
    } else {
        return attacks >> (8 * (y - 2));
    }
}

constexpr bool is_board_valid_debug(const Board brd) {
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

    return is_valid;
}

// checks if there are multiple bitboards with the same bits set
// for any of the squares

// TODO replace with simpler algorithm (running bitset of occupied bits)...
constexpr bool is_board_valid(const Board brd) {
    constexpr auto evalbits = [](u64 a, u64 b, u64 c, u64 d) {
        u64 a1 = c | (a & b) | (a & d) | (b & d);
        u64 a2 = a | b | d;

        return std::make_tuple(a1, a2);
    };

    auto [a1, a2] = evalbits(0, brd.bitboards[0], 0, brd.bitboards[1]);
    auto [b1, b2] = evalbits(0, brd.bitboards[2], 0, brd.bitboards[3]);
    auto [c1, c2] = evalbits(0, brd.bitboards[4], 0, brd.bitboards[5]);
    auto [d1, d2] = evalbits(0, brd.bitboards[6], 0, brd.bitboards[7]);
    auto [e1, e2] = evalbits(0, brd.bitboards[8], 0, brd.bitboards[9]);
    auto [f1, f2] = evalbits(0, brd.bitboards[10], 0, brd.bitboards[11]);

    auto [x1, x2] = evalbits(a1, a2, b1, b2);
    auto [y1, y2] = evalbits(c1, c2, d1, d2);
    auto [z1, z2] = evalbits(e1, e2, f1, f2);
    auto [p1, p2] = evalbits(x1, x2, y1, y2);
    auto [q1, q2] = evalbits(p1, p2, z1, z2);

    return (q1 & q2) == 0;
}

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

    constexpr u64 rank = ((1ull << 8) - 1) << (64 - 8);
    constexpr u64 file = broadcast_byte((u8)1 << 7);

    return (rank >> (8 * y_idx)) ^ (file >> x_idx);
}

void print_bits(auto x, bool print_32 = false) {
    if (print_32) {
        std::cout << std::bitset<32>(x) << '\n';
    } else {
        std::cout << std::bitset<8>(x) << '\n';
    }
}
// void print_bits(auto x) { std::cout << std::bitset<8>(x) << '\n'; }

// TODO apparently can be done with blsr instruction x86
void bit_loop(u64 bits) {
    while (bits) {
        const auto lzcnt = std::countl_zero(bits);

        // use lzcnt (index) here
        std::cout << lzcnt << '\n';

        bits &= ~(msb >> lzcnt);
    }
}

u8 fix_bits_rank(u8 occup, const u8 idx) {
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
u64 ext8bits(u64 x) {
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
u64 dep8bits(u64 x) {
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
u64 fix_bits_file(u64 occup, const u8 sqr_idx) {
    const u8 x_idx = sqr_idx % 8;
    const u8 y_idx = sqr_idx / 8;

    u64 occup_file1 = (occup << x_idx) & broadcast_byte(1 << 7);
    u8 file1 = (u8)(ext8bits(occup_file1) & 0xff);

    return dep8bits(fix_bits_rank(file1, y_idx)) >> x_idx;
}

// TODO does C++ ensure l/r shift shifts into zeros?

// TODO just template over piece color
u64 wpawns_atk(u64 wp, u64 occup, u64 black) noexcept {
    // TODO add sideway attacks
    const u64 fwd_mvs = (wp << 8) | ((wp & RANK2) << 16);

    u64 side_atks =
        ((wp << 9) & broadcast_byte(1)) | ((wp << 7) & broadcast_byte(1 << 7));

    return (fwd_mvs & ~occup) | (side_atks & black);
}

constexpr u64 rook_attacks_fixed(u64 occup, const u8 sqr_idx) noexcept {
    const u8 x_idx = sqr_idx % 8;
    const u8 y_idx = sqr_idx / 8;

    const u8 shift = 8 * (7 - y_idx);
    return ((u64)(fix_bits_rank(occup >> shift, x_idx)) << shift) ^
           fix_bits_file(occup, sqr_idx);
}

#if defined(__X86_64__)
void make_wn_move_avx512(Board& brd, const u8 from_idx, const u8 to_idx) {
    const u64 mask_unset_old = ~(msb >> from_idx);
    // unset bit (from square)
    // might be possible to SIMD this

    // const auto mask1 = _mm256_setr_epi64x(~0ull, mask_unset_old,
    // mask_unset_old,
    //                                       mask_unset_old);

    // no gain from simd here it seems
    // registers are just too far apart in memory and not enough ands/ors
    // are being done to justify simd for this part

    brd.wn() &= mask_unset_old;

    brd.white() &= mask_unset_old;
    brd.occup() &= mask_unset_old;

    // unset bit
    brd.black() &= mask_unset_old;

    const u64 new_knight = msb >> to_idx;
    brd.wn() |= new_knight;
    brd.white() |= new_knight;
    brd.occup() |= new_knight;

    // 6 that need to be checked...
    const auto bbs = _mm512_loadu_si512((__m256i*)(&brd.bitboards[6]));
    const auto new_knights = _mm512_set1_epi64(new_knight);

    // gcc doesn't like static_cast
    // maybe it should be reinterpret_cast instead?

    // TODO do we need to actually mask out the top 2 bits?
    // the tests still don't fail without it...
    u8 mask = _mm512_cmpeq_epi64_mask(_mm512_and_si512(bbs, new_knights),
                                      new_knights) &
              MASK6;

    // I think this works? although not guaranteed branchless...
    auto idx = mask == 0 ? 13 : 6 + std::countr_zero(mask);

    brd.bitboards[idx] &= ~new_knight;
}
#endif

// make white knight move
// will generalize to all applicable moves types later
constexpr void make_wn_move(Board& brd, const u8 from_idx, const u8 to_idx) {
    // TODO maybe add debug_asserts to check for self-capture

    const u64 old_knight = msb >> from_idx;

    // must update
    brd.wn() &= ~old_knight;
    brd.white() &= ~old_knight;
    brd.occup() &= ~old_knight;

    // need to do search now to figure out what piece that was
    // search bitboard index [6, 11]

    const u64 new_knight = msb >> to_idx;
    // must update
    brd.wn() |= new_knight;
    brd.white() |= new_knight;

    auto capture = brd.black() & new_knight;

    if (capture) {
        u64 b1 = brd.bitboards[6] & new_knight;
        // wow... clang auto vectorizes below 4 rotates very smartly
        u64 b2 = std::rotr(brd.bitboards[7] & new_knight, 1);
        u64 b3 = std::rotr(brd.bitboards[8] & new_knight, 2);
        u64 b4 = std::rotr(brd.bitboards[9] & new_knight, 3);
        u64 b5 = std::rotr(brd.bitboards[10] & new_knight, 4);
        u64 b6 = std::rotr(brd.bitboards[11] & new_knight, 5);

        u64 bits = std::rotl(b1 | b2 | b3 | b4 | b5 | b6, to_idx);

        auto idx = std::countl_zero(bits);

        brd.bitboards[6 + idx] &= ~new_knight;
    } else {
        // no need to update if capture, since this bit was already set by old
        // black piece
        brd.occup() |= new_knight;
    }

    // a valid bitboard shouldn't have overlapping bits
    // in these bits except for wn which is index 1

    // search is not necessary for no capture cah
}
