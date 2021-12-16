#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define BUFFER_SIZE 1000

uint8_t input_buffer[BUFFER_SIZE];
size_t input_i = 0;
uint8_t input_left_in_byte = 8;

uint64_t version_sum = 0;

void parse_input(char* filename);
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

uint64_t peek_bits(size_t n) {
    size_t saved_input_i = input_i;
    uint8_t saved_input_left_in_byte = input_left_in_byte;
    uint64_t result = consume_bits(n);
    input_i = saved_input_i;
    input_left_in_byte = saved_input_left_in_byte;
    return result;
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

void parse_operator_body() {
    uint64_t length_type_id = consume_bits(1);
    if (length_type_id == 0) {
        uint64_t length_of_subpackets = consume_bits(15);
        size_t current_cursor = input_cursor_bits();
        while (input_cursor_bits() - current_cursor < length_of_subpackets) {
            parse_packet();
        }
        if (input_cursor_bits() - current_cursor != length_of_subpackets) {
            printf("Expected to consume %lu bits, but consumed %lu bits\n",
                   length_of_subpackets, input_cursor_bits() - current_cursor);
            panic("operator innards length mismatch");
        }
    } else {
        uint64_t number_of_subpackets = consume_bits(11);
        while (number_of_subpackets > 0) {
            parse_packet();
            number_of_subpackets--;
        }
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
        parse_literal_body();
    } else {
        parse_operator_body();
    }
}

int main() {
    parse_input("input");
    parse_packet();
    printf("Part 1: %lu\n", version_sum);
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

void parse_input(char* filename) {
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
