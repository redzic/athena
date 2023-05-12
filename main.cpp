#include <iostream>
#include <cstdint>
#include <tuple>
#include <cassert>

#define u64 uint64_t
#define u32 uint32_t
#define u16 uint16_t
#define u8 uint8_t

#define i64 int64_t
#define i32 int32_t
#define i16 int16_t
#define i8 int8_t

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

    u64 *wp() { return &bitboards[0]; }
    u64 *wn() { return &bitboards[1]; }
    u64 *wr() { return &bitboards[2]; }
    u64 *wb() { return &bitboards[3]; }
    u64 *wq() { return &bitboards[4]; }
    u64 *wk() { return &bitboards[5]; }
    u64 *bp() { return &bitboards[6]; }
    u64 *bn() { return &bitboards[7]; }
    u64 *br() { return &bitboards[8]; }
    u64 *bb() { return &bitboards[9]; }
    u64 *bq() { return &bitboards[10]; }
    u64 *bk() { return &bitboards[11]; }
    u64 *white() { return &bitboards[12]; }
    u64 *black() { return &bitboards[13]; }
    u64 *occup() { return &bitboards[14]; }

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
    u64 wp = (u64)((1 << 8) - 1) << 8;
    u64 bp = (u64)((1 << 8) - 1) << (56 - 8);
    u64 wr = (u64)0b10000001;
    u64 br = (u64)0b10000001 << 56;
    u64 wn = (u64)0b01000010;
    u64 bn = (u64)0b01000010 << 56;
    u64 wb = (u64)0b00100100;
    u64 bb = (u64)0b00100100 << 56;
    u64 wq = (u64)0b00010000;
    u64 bq = (u64)0b00010000 << 56;
    u64 wk = (u64)0b00001000;
    u64 bk = (u64)0b00001000 << 56;

    Board b(wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk);

    return b;
}

constexpr const u8 MASK6 = (1 << 6) - 1;

struct Move
{
    u16 bits;

public:
    u8 from_idx() { return bits & MASK6; }
    u8 to_idx() { return (bits << 6) & MASK6; }
    u8 tag_bits() { return (bits << 12); }

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

u64 knight_attacks_slow(Board &brd, const u8 sqr_idx)
{
    u64 attack_map = 0;

    const std::tuple<i32, i32> knight_offsets[8] = {
        std::make_tuple(1, 2),
        std::make_tuple(2, 1),
        std::make_tuple(-1, 2),
        std::make_tuple(-2, 1),
        std::make_tuple(1, -2),
        std::make_tuple(2, -1),
        std::make_tuple(-1, -2),
        std::make_tuple(-2, -1),
    };

    const int x_idx = sqr_idx % 8;
    const int y_idx = sqr_idx / 8;

    for (int i = 0; i < 8; i++)
    {
        const auto tup = knight_offsets[i];

        const int new_x = x_idx + std::get<0>(tup);
        const int new_y = y_idx + std::get<1>(tup);

        if ((new_x >= 0 && new_x <= 7) && (new_y >= 0 && new_y <= 7))
        {
            u64 sqr = (((u64)1 << 63) >> (8 * new_y)) >> new_x;
            attack_map |= sqr;
        }
    }

    return attack_map;
}

// assume white pieces
u64 knight_attacks(Board &brd, const u8 sqr_idx)
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

    const u64 N_ATTACK_BITS = 0b01010000'10001000'00000000'10001000'01010000'00000000'00000000'00000000;

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

    const u64 FIX_OOB = 0b11110000'11110000'11110000'11110000'11110000'11110000'11110000'11110000;

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

// assume white pieces
u64 knight_attacks_fast(Board &brd, const u8 sqr_idx)
{
    // TODO figure out how to make debug assert in C++
    // debug_assert(sqr_idx <= 63);
    u64 attack_map = 0;

    // center = 2,2 = 18

    // ORIGINAL:

    // 01010000
    // 10001000
    // 00000000
    // 10001000
    // 01010000
    // 00000000
    // 00000000
    // 00000000

    // when you shift left by n bits, you want
    // to mask out the bits on the right

    // 01000010
    // 00100000
    // 00000010
    // 00100001
    // 01000000
    // 00000000
    // 00000000
    // 00000000

    // so since we shifted left by 2, mask would look like this:
    // 11111100
    // 11111100
    // 11111100
    // 11111100
    // 11111100
    // 11111100
    // 11111100
    // 11111100

    // P1:

    // 01010000
    // 10001000
    // 00000000
    // 00000000
    // 00000000
    // 00000000
    // 00000000
    // 00000000

    // P2:

    // 10001000
    // 01010000
    // 00000000
    // 00000000
    // 00000000
    // 00000000
    // 00000000
    // 00000000

    // RIGHT:

    // 01000000
    // 00100000
    // 00000000
    // 00100000
    // 01000000
    // 00000000
    // 00000000
    // 00000000

    // LEFT:

    // 00000010
    // 00000100
    // 00000000
    // 00000100
    // 00000010
    // 00000000
    // 00000000
    // 00000000

    const u64 DUP8_BITS = 0x101010101010101;

    const u64 N_ATTACK_LEFT = 0b00000010'00000100'00000000'00000100'00000010'00000000'00000000'00000000;
    const u64 N_ATTACK_RIGHT = 0b01000000'00100000'00000000'00100000'01000000'00000000'00000000'00000000;

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

bool is_board_valid_simple(Board bitboard)
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
bool is_board_valid(Board bitboard)
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

void test_is_board_valid()
{
    // Test case 1: starting position - should return true
    Board starting_position = Board::starting_position();
    std::cout << "Test 1: " << std::boolalpha << is_board_valid(starting_position) << std::endl;

    // Test case 2: two pawns on the same square - should return false
    Board invalid_position = starting_position;
    *invalid_position.wp() |= (u64)((1 << 8) - 1) << (56 - 8); // Adding a white pawn to the same square as a black pawn
    std::cout << "Test 2: " << std::boolalpha << is_board_valid(invalid_position) << std::endl;

    // Test case 3: two queens on the same square - should return false
    Board invalid_position_2 = starting_position;
    *invalid_position_2.wq() |= (u64)0b00010000 << 56; // Adding a white queen to the same square as a black queen
    std::cout << "Test 3: " << std::boolalpha << is_board_valid(invalid_position_2) << std::endl;
}

int main()
{
    auto brd = Board::starting_position();
    // TODO make this ((u64)1 << 63) thing a constant
    // like a1, b1, c1, ...

    // x = 0,1,6,7
    // need filtering

    const u8 x = 7;
    const u8 y = 4;
    const u8 idx = 8 * y + x;

    // *brd.wn() |= ((u64)1 << 63) >> idx;

    // print_bitboard(*brd.wn());

    u64 attacks = knight_attacks_fast(brd, idx);
    print_bitboard(attacks);

    for (int idx = 0; idx < 64; idx++)
    {
        u64 a1 = knight_attacks(brd, idx);
        u64 a2 = knight_attacks_slow(brd, idx);
        u64 a3 = knight_attacks_fast(brd, idx);

        print_bitboard(a1);
        std::cout << "==========================\n";

        assert(a1 == a2);
        assert(a1 == a3);
    }
}
