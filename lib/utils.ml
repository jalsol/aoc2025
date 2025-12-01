type part =
  | Part1
  | Part2

let raw_input path = In_channel.with_open_text path In_channel.input_all
let raw_lines path = In_channel.with_open_text path In_channel.input_lines
