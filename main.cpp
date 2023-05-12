#include <iostream>
#include <cstdint>
#include <tuple>

#define u64 uint64_t
#define u32 uint32_t
#define u16 uint16_t
#define u8 uint8_t

#define i64 int64_t
#define i32 int32_t
#define i16 int16_t
#define i8 int8_t

struct Board
{
    // wp, wn, wr, wb, wq, wk, bp, bn, br, bb, bq, bk
    u64 bitboards[12];

public:
    // returns new board of standard starting chess position
    static Board starting_position();

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
};

Board Board::starting_position()
{
    Board b;

    *b.wp() = ((u64)((1 << 8) - 1) << 8);
    *b.bp() = ((u64)((1 << 8) - 1) << (56 - 8));

    *b.wr() = ((u64)0b10000001);
    *b.br() = ((u64)0b10000001 << 56);

    *b.wn() = ((u64)0b01000010);
    *b.bn() = ((u64)0b01000010 << 56);

    *b.wb() = ((u64)0b00100100);
    *b.bb() = ((u64)0b00100100 << 56);

    *b.wq() = ((u64)0b00010000);
    *b.bq() = ((u64)0b00010000 << 56);

    *b.wk() = ((u64)0b00001000);
    *b.bk() = ((u64)0b00001000 << 56);

    return b;
}

constexpr const u8 MASK6 = (1 << 6) - 1;

struct Move
{
    u16 bits;

public:
    u8 from_idx()
    {
        return bits & MASK6;
    }

    u8 to_idx()
    {
        return (bits << 6) & MASK6;
    }

    u8 tag_bits()
    {
        return (bits << 12);
    }

    constexpr Move(u8 from, u8 to, u8 tag);
};

constexpr Move::Move(u8 from, u8 to, u8 tag)
{
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
        }

        bitboard <<= 8;

        puts("");
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

        if (count > 1)
        {
            return false;
        }
    }

    return true;
}

bool is_board_valid(Board bitboard)
{
    auto evalbits = [](u64 a, u64 b, u64 c, u64 d)
    {
        u64 a1 = (c & d) | (b & d) | (b & c) | (a & d) | (a & c) | (a & b);
        u64 a2 = a | b | c | d;
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
    // auto b = Board::starting_position();

    // // print_bitboard(b.wp);
    // std::cout << is_board_valid(b);

    test_is_board_valid();
}
