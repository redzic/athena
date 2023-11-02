fen_str = "pnrqkbPNRQKB"


def string_to_bytes_array(s):
    return [ord(character) for character in s]


fen_b = string_to_bytes_array(fen_str)
print(min(fen_b), max(fen_b))
