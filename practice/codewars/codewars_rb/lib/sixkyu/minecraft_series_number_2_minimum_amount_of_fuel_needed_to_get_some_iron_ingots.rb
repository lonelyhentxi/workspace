# frozen_string_literal: true

def min_fuel_recur(fuel_units, fuel_time, current_index)
  if fuel_time <= 0
    [0, Array.new(fuel_units.length - 1 - current_index, 0)]
  elsif current_index == fuel_units.length - 1
    optimized_num = (fuel_time / fuel_units[current_index].to_f).ceil
    [optimized_num * fuel_units[current_index], [optimized_num]]
  else
    max_num = (fuel_time / fuel_units[current_index].to_f).ceil
    optimized_num = Float::INFINITY
    optimized_trace = nil
    max_num.downto(0).each do |num|
      current_num_self_time = num * fuel_units[current_index]
      remain_res = min_fuel_recur(fuel_units, fuel_time - current_num_self_time, current_index + 1)
      current_num_optimized_time = current_num_self_time + remain_res[0]
      next unless current_num_optimized_time < optimized_num
      optimized_trace = remain_res[1]
      optimized_trace.push(num)
      optimized_num = current_num_optimized_time
    end
    [optimized_num, optimized_trace]
  end
end

def calc_fuel(n)
  fuel_time = n * 11
  fuel_info = {lava: 800, blaze_rod: 120, coal: 80, wood: 15, stick: 1}
  fuel_units = fuel_info.values
  fuel_names = fuel_info.keys
  optimized_res = min_fuel_recur(fuel_units, fuel_time, 0)
  Hash[*optimized_res[1].reverse!.map!.with_index { |x, i| [fuel_names[i], x] }.flatten]
end
