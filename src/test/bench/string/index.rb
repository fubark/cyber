str = 'abcdefghijklmnopqrstuvwxyz123456' * 1000000 + 'waldo'

start = Time.now
idx = 0
50.times { |i|
    idx = str.index('waldo')
}
puts "idx: #{idx} ms: #{(Time.now - start) * 1000}"