# frozen_string_literal: true

def blocks_to_collect(level)
  material_names = %i[gold diamond emerald iron]
  material_counts = Array.new(4, 0)
  (0...level).each do |i|
    index = i % 4
    material_counts[index] += (1 + (1 + i) * 2).pow(2)
  end
  material_names.push :total
  material_counts.push material_counts.sum
  Hash[*material_counts.map.with_index { |v, i| [material_names[i], v] }.flatten]
end
