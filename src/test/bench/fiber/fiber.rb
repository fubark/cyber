$count = 0

fibers = []
100000.times do |i| 
    f = Fiber.new do 
        $count += 1
        Fiber.yield
        $count += 1
    end
    f.resume()
    fibers << f
end

fibers.each {|f| f.resume() }

puts $count