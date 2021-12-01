defmodule Day1 do
  def part_1 do
    ints = File.read!("./input") |> String.split("\n") |> Enum.map(&String.to_integer/1)

    Enum.chunk_every(ints, 2, 1, :discard)
    |> Enum.map(fn [a, b] -> if b > a, do: 1, else: 0 end)
    |> Enum.sum()
  end

  def part_2 do
    ints = File.read!("./input") |> String.split("\n") |> Enum.map(&String.to_integer/1)
    chunks = Enum.chunk_every(ints, 3, 1, :discard)
    chunk_sums = chunks |> Enum.map(&Enum.sum/1)

    Enum.chunk_every(chunk_sums, 2, 1, :discard)
    |> Enum.map(fn [a, b] -> if b > a, do: 1, else: 0 end)
    |> Enum.sum()
  end

  def go do
    IO.inspect(part_1())
    IO.inspect(part_2())
  end
end
