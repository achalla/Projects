threads1 = []
threads2 = []
200.times do |i|
	threads1[i] = Thread.new{
		sleep 0.001
		print "1"
	}
end

#threads1.size.times do |i| threads1[i].join end

200.times do |i|
	threads2[i] = Thread.new{
		sleep 0.001
		print "2"
	}
end

threads1.size.times do |i| threads1[i].join end
threads2.size.times do |i| threads2[i].join end

