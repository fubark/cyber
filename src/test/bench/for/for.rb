start = Time.now

list = []
1000000.times {|i| list << i}

sum = 0
list.each {|i| sum += i}

puts "time: " + ((Time.now - start) * 1000).to_s
puts sum