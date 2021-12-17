#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>

#define BUFFER_SIZE 1000

uint8_t input_buffer[BUFFER_SIZE];
size_t input_i = 0;
uint8_t input_left_in_byte = 8;

uint64_t version_sum = 0;

void parse_input(char *filename);
uint64_t parse_packet();

size_t input_cursor_bits() {
    return (input_i * 8) + (8 - input_left_in_byte);
}

uint64_t consume_bits(size_t n) {
    uint64_t acc = 0;
    for (; n > 0; n--) {
        if (input_left_in_byte == 0) {
            input_left_in_byte = 8;
            input_i++;
        }
        acc = acc << 1;
        acc = acc | ((input_buffer[input_i] & 0b10000000) >> 7);
        input_buffer[input_i] = input_buffer[input_i] << 1;
        input_left_in_byte--;
    }
    return acc;
}

void panic(const char *message) {
    printf("%s\n", message);
    exit(1);
}

uint64_t parse_literal_body() {
    uint64_t acc = 0;
    while (consume_bits(1)) {
        acc <<= 4;
        acc |= consume_bits(4);
    }
    // the final bits
    acc <<= 4;
    acc |= consume_bits(4);
    return acc;
}

size_t parse_subpackets_bitlength(uint64_t bitlength, uint64_t *subpackets) {
    size_t num_subpackets = 0;

    size_t current_cursor = input_cursor_bits();
    while (input_cursor_bits() - current_cursor < bitlength) {
        subpackets[num_subpackets++] = parse_packet();
    }
    assert(input_cursor_bits() - current_cursor == bitlength);

    return num_subpackets;
}

uint64_t parse_operator_body(uint8_t type_id) {
    uint64_t length_type_id = consume_bits(1);
    size_t num_subpackets;
    uint64_t subpackets[100];
    if (length_type_id == 0) {
        uint64_t length_of_subpackets = consume_bits(15);
        num_subpackets = parse_subpackets_bitlength(length_of_subpackets, subpackets);
    } else {
        num_subpackets = 0;
        for (uint64_t rem = consume_bits(11); rem > 0; rem--) {
            subpackets[num_subpackets++] = parse_packet();
        }
    }

    switch (type_id) {
        // sum
        case 0: {
            uint64_t acc = 0;
            for (; num_subpackets > 0; num_subpackets--) {
                acc += subpackets[num_subpackets - 1];
            }
            return acc;
        }
        // product
        case 1: {
            uint64_t acc = 1;
            for (; num_subpackets > 0; num_subpackets--) {
                acc *= subpackets[num_subpackets - 1];
            }
            return acc;
        }
        // min
        case 2: {
            uint64_t min = UINT64_MAX;
            for (; num_subpackets > 0; num_subpackets--) {
                min = MIN(min, subpackets[num_subpackets - 1]);
            }
            return min;
        }
        // max
        case 3: {
            uint64_t max = 0;
            for (; num_subpackets > 0; num_subpackets--) {
                max = MAX(max, subpackets[num_subpackets - 1]);
            }
            return max;
        }
        // greater than
        case 5: {
            assert(num_subpackets == 2);
            return subpackets[0] > subpackets[1];
        }
        // less than
        case 6: {
            assert(num_subpackets == 2);
            return subpackets[0] < subpackets[1];
        }
        // equal
        case 7: {
            assert(num_subpackets == 2);
            return subpackets[0] == subpackets[1];
        }
        default:
            printf("Unknown operator type id %u\n", type_id);
            panic("unknown operator type");
    }
}

uint8_t parse_header() {
    uint64_t version_num = consume_bits(3);
    version_sum += version_num;
    return consume_bits(3);
}

// returns the version number
uint64_t parse_packet() {
    uint8_t type_id = parse_header();

    if (type_id == 4) {
        return parse_literal_body();
    } else {
        return parse_operator_body(type_id);
    }
}

int main() {
    parse_input("input");
    uint64_t result = parse_packet();
    printf("Part 1: %lu\n", version_sum);
    printf("Part 2: %lu\n", result);
    return 0;
}

uint8_t nib_from_hex(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else {
        return 255;
    }
}

void parse_input(char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        panic("couldn't find file");
    }
    size_t i = 0;
    uint8_t parsed;
    while (true) {
        parsed = nib_from_hex((char) getc(file));
        if (parsed == 255) {
            break;
        }
        input_buffer[i] = parsed;

        parsed = nib_from_hex((char) getc(file));
        if (parsed == 255) {
            break;
        }
        input_buffer[i] = (input_buffer[i] << 4) | parsed;

        i++;

        if (i == BUFFER_SIZE) {
            panic("Input buffer overflow");
        }
    }
}
