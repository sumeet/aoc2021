defmodule Day1 do
  def go do
    ints = File.read!("./input") |> String.split("\n") |> Enum.map(&String.to_integer/1)
    seconds = Enum.drop(ints, 1) |> Enum.with_index()

    count =
      Enum.map(seconds, fn {n, i} ->
        if n > Enum.at(ints, i), do: 1, else: 0
      end)
      |> Enum.sum()

    IO.inspect(count)
  end
end
