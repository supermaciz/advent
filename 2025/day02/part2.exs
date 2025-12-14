defmodule Day02Part2 do
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
    [first, last] = str |> String.split("-") |> Enum.map(&String.trim/1)
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

    # |> IO.inspect(label: "Invalid ids in #{inspect(range)}", charlists: :as_lists)
  end

  defp invalid?(<<_>>), do: false

  defp invalid?(int_str) do
    len = String.length(int_str)
    max_substring_len = Integer.floor_div(len, 2)
    digits = String.graphemes(int_str)

    Enum.any?(1..max_substring_len, fn
      window_len when rem(len, window_len) != 0 ->
        false

      window_len ->
        chunks = Enum.chunk_every(digits, window_len)
        Enum.all?(chunks, &(&1 == hd(chunks)))
    end)
  end
end

[file | _] = System.argv()
result = Day02Part2.solve(file)
IO.puts("Result: #{result}")
