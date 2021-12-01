defmodule Day1 do
  def part_1 do
    ints = File.read!("./input") |> String.split("\n") |> Enum.map(&String.to_integer/1)

    seconds = Enum.drop(ints, 1) |> Enum.with_index()

    Enum.map(seconds, fn {n, i} ->
      if n > Enum.at(ints, i), do: 1, else: 0
    end)
    |> Enum.sum()
  end

  def part_2 do
    ints = File.read!("./input") |> String.split("\n") |> Enum.map(&String.to_integer/1)
    chunks = Enum.chunk_every(ints, 3, 1, :discard)
    chunk_sums = chunks |> Enum.map(&Enum.sum/1)
    seconds = Enum.drop(chunk_sums, 1) |> Enum.with_index()

    Enum.map(seconds, fn {n, i} ->
      if n > Enum.at(chunk_sums, i), do: 1, else: 0
    end)
    |> Enum.sum()
  end

  def go do
    IO.inspect(part_2())
  end
end
