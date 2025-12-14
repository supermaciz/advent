defmodule Day02Part1 do
  require Integer

  def solve(file) do
    input = File.read!(file)

    input
    |> String.split(",")
    |> Stream.map(&string_to_range/1)
    |> Stream.map(&find_invalid_ids/1)
    |> Stream.concat()
    |> Enum.sum()
  end

  defp string_to_range(str) do
    [first, last] = String.split(str, "-")
    String.to_integer(first)..String.to_integer(last)
  end

  @spec find_invalid_ids(Range.t()) :: [integer]
  defp find_invalid_ids(range) do
    range
    |> Stream.map(&{&1, Integer.to_string(&1)})
    |> Enum.reduce([], fn {int, int_str}, acc ->
      if invalid?(int_str) do
        [int | acc]
      else
        acc
      end
    end)
    |> IO.inspect(label: "Invalid ids in #{inspect(range)}", charlists: :as_lists)
  end

  defp invalid?(int_str) do
    len = String.length(int_str)

    if Integer.is_odd(len) do
      false
    else
      [first, last] =
        int_str
        |> String.graphemes()
        |> Enum.chunk_every(Integer.floor_div(len, 2))

      first == last
    end
  end
end

[file | _] = System.argv()
result = Day02Part1.solve(file)
IO.puts("Result: #{result}")
