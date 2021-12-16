#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>

#define BUFFER_SIZE 1000

int8_t INPUT_BUFFER [BUFFER_SIZE];
int64_t VERSION_SUM = 0;

void parse_input();

void panic (const char *message) {
    printf ("%s\n", message);
    exit(1);
}



int main() {
    parse_input();

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

void parse_input() {
    FILE *file = fopen("input", "r");
    size_t input_i = 0;
    uint8_t parsed;
    while (true) {
        parsed = nib_from_hex((char) getc(file));
        if (parsed == 255) {
            break;
        }
        INPUT_BUFFER[input_i] = parsed;

        parsed = nib_from_hex((char) getc(file));
        if (parsed == 255) {
            break;
        }
        INPUT_BUFFER[input_i] = (INPUT_BUFFER[input_i] << 4) | parsed;

        input_i++;

        if (input_i == BUFFER_SIZE) {
            panic("Input buffer overflow");
        }
    }
}
