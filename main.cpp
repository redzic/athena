#include <iostream>
#include <cstdint>
#include <tuple>
#include <cassert>
#include <array>

using u64 = std::uint64_t;
using u32 = std::uint32_t;
using u16 = std::uint16_t;
using u8 = std::uint8_t;

using i64 = std::int64_t;
using i32 = std::int32_t;
using i16 = std::int16_t;
using i8 = std::int8_t;

enum PieceColor
{
    White = 0,
    Black = 1
};

struct Board
{
    // wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk, white, black, occupied (white and black)
    u64 bitboards[12 + 3];

public:
    // returns new board of standard starting chess position
    static constexpr Board starting_position();

    // TODO somehow figure out const version

    constexpr u64 &wp() { return bitboards[0]; }
    constexpr u64 &wn() { return bitboards[1]; }
    constexpr u64 &wr() { return bitboards[2]; }
    constexpr u64 &wb() { return bitboards[3]; }
    constexpr u64 &wq() { return bitboards[4]; }
    constexpr u64 &wk() { return bitboards[5]; }
    constexpr u64 &bp() { return bitboards[6]; }
    constexpr u64 &bn() { return bitboards[7]; }
    constexpr u64 &br() { return bitboards[8]; }
    constexpr u64 &bb() { return bitboards[9]; }
    constexpr u64 &bq() { return bitboards[10]; }
    constexpr u64 &bk() { return bitboards[11]; }
    constexpr u64 &white() { return bitboards[12]; }
    constexpr u64 &black() { return bitboards[13]; }
    constexpr u64 &occup() { return bitboards[14]; }

    constexpr Board(
        u64 wp,
        u64 wn,
        u64 wr,
        u64 wb,
        u64 wq,
        u64 wk,
        u64 bp,
        u64 bn,
        u64 br,
        u64 bb,
        u64 bq,
        u64 bk)
    {
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
};

constexpr Board Board::starting_position()
{
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

struct Move
{
    u16 bits;

public:
    constexpr u8 from_idx() { return bits & MASK6; }
    constexpr u8 to_idx() { return (bits << 6) & MASK6; }
    constexpr u8 tag_bits() { return (bits << 12); }

    constexpr Move(u8 from, u8 to, u8 tag);
};

constexpr Move::Move(u8 from, u8 to, u8 tag)
{
    // it's saying this was not constructed/initialized?
    bits = (from & MASK6) | ((to & MASK6) >> 6) | ((tag & 0b1111) >> 12);
}

void print_bitboard(u64 bitboard)
{
    for (auto i = 0; i < 8; i++)
    {
        for (auto j = 0; j < 8; j++)
        {
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
consteval u64 knight_attack_map(const u8 sqr_idx)
{
    u64 attack_map = 0;

    const std::tuple<int, int> knight_offsets[8] = {
        {1, 2},
        {2, 1},
        {-1, 2},
        {-2, 1},
        {1, -2},
        {2, -1},
        {-1, -2},
        {-2, -1},
    };

    const int x_idx = sqr_idx % 8;
    const int y_idx = sqr_idx / 8;

    for (const auto &[dx, dy] : knight_offsets)
    {
        const int new_x = x_idx + dx;
        const int new_y = y_idx + dy;

        if ((new_x >= 0 && new_x <= 7) && (new_y >= 0 && new_y <= 7))
        {
            u64 sqr = ((1ull << 63) >> (8 * new_y)) >> new_x;
            attack_map |= sqr;
        }
    }

    return attack_map;
}

consteval std::array<u64, 64> build_knight_table()
{
    std::array<u64, 64> table;

    for (auto i = 0; i < 64; i++)
    {
        table[i] = knight_attack_map(i);
    }

    return table;
}

static constexpr auto KNIGHT_ATTACK_TABLE = build_knight_table();

// assume white pieces

constexpr u64 knight_attacks(const Board &brd, const u8 sqr_idx)
{
    return KNIGHT_ATTACK_TABLE[sqr_idx];
}

// seems best on clang since the compiled code is branchless
constexpr u64 knight_attacks_bitwise(const Board &brd, const u8 sqr_idx)
{
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

    constexpr u64 N_ATTACK_BITS = 0b01010000'10001000'00000000'10001000'01010000'00000000'00000000'00000000;

    int dist = 18 - (int)sqr_idx;

    // i.e. center > sqr_idx
    if (dist > 0)
    {
        attack_map = N_ATTACK_BITS << dist;
    }
    else
    {
        attack_map = N_ATTACK_BITS >> (-dist);
    }

    u8 x_idx = sqr_idx % 8;

    constexpr u64 FIX_OOB = 0b11110000'11110000'11110000'11110000'11110000'11110000'11110000'11110000;

    if (x_idx <= 1)
    {
        attack_map &= FIX_OOB;
    }
    else if (x_idx >= 6)
    {
        attack_map &= FIX_OOB >> 4;
    }

    // TODO figure out what happens when king is attacked...
    // surely that can't be a legal move right?

    // return attack_map & (~(*brd.white()));
    return attack_map;
}

consteval u64 broadcast_byte(const u8 b)
{
    return 0x101010101010101ull * static_cast<u64>(b);
}

// assume white pieces
constexpr u64 knight_attacks_fast(const Board &brd, const u8 sqr_idx)
{
    // TODO figure out how to make debug assert in C++
    // debug_assert(sqr_idx <= 63);

    constexpr u64 DUP8_BITS = 0x101010101010101;

    constexpr u64 N_ATTACK_LEFT = 0b00000010'00000100'00000000'00000100'00000010'00000000'00000000'00000000;
    constexpr u64 N_ATTACK_RIGHT = 0b01000000'00100000'00000000'00100000'01000000'00000000'00000000'00000000;

    const u8 x = sqr_idx % 8;
    const u8 y = sqr_idx / 8;

    // auto r_shift = sqr_idx;
    const u64 rmask = ((u64)((u8)(((u8)0b1111'1111) >> x))) * DUP8_BITS;

    const u64 lmask = ((u64)((u8)(((u8)0b1111'1111) << (7 - x)))) * DUP8_BITS;

    u64 right_side = (N_ATTACK_RIGHT >> x) & rmask;
    u64 left_side = (N_ATTACK_LEFT << (7 - x)) & lmask;

    u64 attacks = left_side | right_side;

    if (y <= 1)
    {
        return attacks << (8 * (2 - y));
    }
    else
    {
        return attacks >> (8 * (y - 2));
    }
}

constexpr bool is_board_valid_simple(Board bitboard)
{
    u64 *boards = bitboard.bitboards;

    for (auto i = 0; i < 64; i++)
    {
        int count = 0;

        for (auto j = 0; j < 12; j++)
        {
            // count += ((boards[j] << i) >> 63);
            count += (boards[j] >> (63 - i)) & 1;
        }

        // LLVM optimization idea: if this branch were in the
        // middle of the for loop, the code would still ultimately
        // do the same thing, but autovectorization would not be
        // possible. Some heursitics could possibly be used to
        // judge whether or not the early-exits would be worth it.
        if (count > 1)
        {
            return false;
        }
    }

    return true;
}

// checks if there are multiple bitboards with the same bits set
// for any of the squares

// TODO replace with simpler algorithm (running bitset of occupied bits)...
constexpr bool is_board_valid(Board bitboard)
{
    constexpr auto evalbits = [](u64 a, u64 b, u64 c, u64 d)
    {
        u64 a1 = c | (a & b) | (a & d) | (b & d);
        u64 a2 = a | b | d;

        return std::make_tuple(a1, a2);
    };

    u64 *boards = bitboard.bitboards;

    auto [a1, a2] = evalbits(0, boards[0], 0, boards[1]);
    auto [b1, b2] = evalbits(0, boards[2], 0, boards[3]);
    auto [c1, c2] = evalbits(0, boards[4], 0, boards[5]);
    auto [d1, d2] = evalbits(0, boards[6], 0, boards[7]);
    auto [e1, e2] = evalbits(0, boards[8], 0, boards[9]);
    auto [f1, f2] = evalbits(0, boards[10], 0, boards[11]);

    auto [x1, x2] = evalbits(a1, a2, b1, b2);
    auto [y1, y2] = evalbits(c1, c2, d1, d2);
    auto [z1, z2] = evalbits(e1, e2, f1, f2);
    auto [p1, p2] = evalbits(x1, x2, y1, y2);
    auto [q1, q2] = evalbits(p1, p2, z1, z2);

    return (q1 & q2) == 0;
}

// bro how the heck does this work...
constexpr u64 knight_attacks_multiple(const u64 knights)
{
    u64 l1 = (knights >> 1) & 0x7f7f7f7f7f7f7f7full;
    u64 l2 = (knights >> 2) & 0x3f3f3f3f3f3f3f3full;
    u64 r1 = (knights << 1) & 0xfefefefefefefefeull;
    u64 r2 = (knights << 2) & 0xfcfcfcfcfcfcfcfcull;
    u64 h1 = l1 | r1;
    u64 h2 = l2 | r2;
    return (h1 << 16) | (h1 >> 16) | (h2 << 8) | (h2 >> 8);
}

int main()
{
    auto brd = Board::starting_position();
    // TODO make this ((u64)1 << 63) thing a constant
    // like a1, b1, c1, ...

    // x = 0,1,6,7
    // need masking

    const u8 x = 7;
    const u8 y = 4;
    const u8 idx = 8 * y + x;

    brd.wn() |= (1ull << 63) >> idx;

    print_bitboard(brd.wn());

    // u64 attacks = knight_attacks_fast(brd, idx);
    u64 attacks = knight_attacks_multiple(brd.wn());
    print_bitboard(attacks);
    std::cout << "================================\n";
    print_bitboard(brd.wn());

    for (int idx = 0; idx < 64; idx++)
    {
        u64 a1 = knight_attacks(brd, idx);
        u64 a2 = knight_attacks_bitwise(brd, idx);

        print_bitboard(a1);
        std::cout << "==========================\n";

        assert(a1 == a2);
    }
}
