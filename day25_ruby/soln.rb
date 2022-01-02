require "set"

grid = next_grid = File.read("./sample").split("\n")

(1..).each do |n|
  next_grid = grid.map {|line| "." * line.length}
  any_moves = false
  # east facing moves first
  moved_east = Set.new
  grid.each_with_index do |line, y|
    line.chars.each_with_index do |cell, x|
      right = grid[y][(x+1) % line.length]
      if cell == ">"
        if right == "." # moves to the right
          next_grid[y][(x+1) % line.length] = ">"
          moved_east << [y, (x+1) % line.length]
          any_moves = true
        else # stays
          next_grid[y][x] = ">"
        end
      end
    end
  end

  # south-facing moves last
  grid.each_with_index do |line, y|
    line.chars.each_with_index do |cell, x|
      if cell == "v"
        down = grid[(y+1) % grid.length][x]
        if down == "." && !moved_east.include?([(y+1) % grid.length, x])
          next_grid[(y+1) % grid.length][x] = "v"
          any_moves = true
        elsif down == ">" && moved_east.include?([(y+1) % grid.length, (x+1) % line.length])
          next_grid[(y+1) % grid.length][x] = "v"
          any_moves = true
        else
          next_grid[y][x] = "v"
        end
      end
    end
  end

  grid = next_grid

  unless any_moves
    puts "Part 1: #{n}"
    break
  end
end