import functools
from math import floor, ceil


def explode(s):
    left = []
    brace_cnt = 0
    for c in s:
        match c:
            case '[':
                brace_cnt += 1
            case ']':
                brace_cnt -= 1
                if brace_cnt < 0:
                    raise 'unexpected ]'
            case n if c.isdigit() and brace_cnt > 4:
                next = len(left) + 1
                if s[next] == ',' and s[next + 1].isdigit():
                    # explode left
                    l = n
                    r = s[next + 1]
                    for i, c in enumerate(reversed(left)):
                        i = len(left) - 1 - i
                        if c.isdigit():
                            left[i] = str(int(c) + int(l))
                            break
                    # explode right
                    rest = list(s[len(left) + 4:])
                    for i, c in enumerate(rest):
                        if c.isdigit():
                            rest[i] = str(int(c) + int(r))
                            break
                    return ''.join(left[:-1] + ['0'] + rest)
            case _ if c.isdigit():
                pass
            case ',':
                pass
            case otherwise:
                raise f'unexpected: ${otherwise}'
        left.append(c)

    # no explosion happened
    return None


def split(orig_s):
    s = list(orig_s)
    for i, c in enumerate(s):
        if c.isdigit():
            length = 1
            while orig_s[i + length].isdigit():
                length += 1
            if length > 1:
                num = int(orig_s[i:i + length]) / 2
                l = floor(num)
                r = ceil(num)
                s[i] = f'[{l},{r}]'
                s[i + 1] = ''
                return ''.join(s)
    return None


def reduce(s):
    while True:
        res = explode(s)
        if res:
            s = res
            print("explode", s)
            continue

        res = split(s)
        if res:
            s = res
            print("split", s)
            continue
        break
    return s


def add(l, r):
    return reduce('[' + l + ',' + r + ']')


print(functools.reduce(add, open('sample').read().splitlines()))
