def tokenize(s)
  tokens = []

  i = 0
  while i < s.size
    if s[i].match?(/[0-9]/)
      num = s[i].to_i
      i += 1
      while s[i].match?(/[0-9]/)
        num = num * 10 + s[i].to_i
        i += 1
      end
      tokens << num
    else
      tokens << s[i]
      i += 1
    end
  end

  tokens
end

def explode(tokens)
  bracket_stack = 0
  (0...tokens.size).each do |i|
    c = tokens[i]
    if c == '['
      bracket_stack += 1
    elsif c == ']'
      bracket_stack -= 1
    elsif c.is_a?(Integer) && bracket_stack > 4
      # do explosion
      l = c
      r = tokens[i + 2]

      # explode left
      ((i - 1)..0).step(-1).each do |i|
        if tokens[i].is_a?(Integer)
          tokens[i] += l
          break
        end
      end

      # explode right
      ((i + 3)...tokens.size).each do |i|
        if tokens[i].is_a?(Integer)
          tokens[i] += r
          break
        end
      end

      return tokens[0..(i - 2)] + [0] + tokens[(i + 4)..]
    end
  end

  nil
end

def split(tokens)
  (0...tokens.size).each do |i|
    if tokens[i].is_a?(Integer) && tokens[i] >= 10
      l = (tokens[i] / 2.0).floor
      r = (tokens[i] / 2.0).ceil
      return tokens[0...i] + ['[', l, ',', r, ']'] + tokens[(i + 1)..]
    end
  end
  nil
end

def reduce(tokens)
  loop do
    # first try explode
    exploded = explode(tokens)
    unless exploded.nil?
      tokens = exploded
      next
    end

    # then try split
    split = split(tokens)
    unless split.nil?
      tokens = split
      next
    end

    break
  end
  tokens
end

def add(l, r)
  reduce(['['] + l + [','] + r + [']'])
end

def magnitude(tokens)
  magnitude_arr(eval(tokens.map(&:to_s).join))
end

def magnitude_arr(arr)
  return arr if arr.is_a?(Integer)
  lhs, rhs = arr
  (3 * magnitude_arr(lhs) + 2 * magnitude_arr(rhs))
end

inputs = File.read("input").split("\n")
final = tokenize(inputs.first)
inputs[1..].each do |input|
  final = add(final, tokenize(input))
end
p final.map(&:to_s).join
p magnitude(final)
