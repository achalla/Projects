# CMSC 330 / Summer 2014 / Project 1
# Student: Anu Challa

# Fill in the implementation and submit just this file
	mode = ARGV[0]
	log_file_name = ARGV[1]

	#This is RegEx processing for every task below
	two55_check = '(([0-9])|([0-9][0-9])|(1[0-9][0-9])|(2[0-4][0-9])|(25[0-5]))'
	ip_check = two55_check+'\.'+two55_check+'\.'+two55_check+'\.'+two55_check
	username_check = '(\w+|-)'
	dmy_check = '\[([012][0-9]|3[01])\/(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\/[0-9]{4}'
	hms_check = ':([01][0-9]|2[0123]):[0-5][0-9]:[0-5][0-9] -0400\]'
	date_check = dmy_check+hms_check
	request_check = '("([^"]|\\\")*")'
	status_check = '(\d+)'
	bytes_check = '(\d+|-)'
	#72.30.61.37 - - [22/Jul/2006:00:00:15 -0400] "GET /~ben/ HTTP/1.0" 200 10175
	total_re = Regexp.new('^' + ip_check + ' - ' + username_check + ' ' + date_check + ' ' + request_check + ' ' + status_check + ' ' + bytes_check + '$')
	#1.1.1.1 - achalla [01/Feb/1999:23:59:59 -0400] "meow" 8 -

	#validate setup
	valid = true
	
	#bytes setup
	if (mode == "bytes" ) then bytes = 0 end
	
	#time & popularity setup
	if(mode == "time" or mode == "popularity") 
		h = Hash.new(0)
	end
	
	#requests setup
	if(mode == "requests")
		h = Hash.new(0)
		b = Hash.new(0)
		r = []
	end

#master loop: file line iteration
File.open(log_file_name,"r") do |file|
	while !file.eof
		val = file.readline		
			if val =~ total_re
				#bytes execution	
				if (mode == "bytes" ) 
					bytes += $32.to_i
				end
				#time execution
				if(mode == "time") 
					h[$28.to_s] += 1;
				end
				#popularity execution
				if(mode == "popularity") 
					h[$29.to_s] += 1;
				end
				#requests execution
				if(mode == "requests") 
					 h[(val.split(" ")[0])] += 1
					 b[(val.split(" ")[0])] += $32.to_i
				end
			else 
				valid = false
			end
	end
end
	

#validate epilogue	
if (mode == "validate")
	if(valid == true) 
		print "yes\n"
		puts $29,$30,$31
	else print "no\n"
	end
end

#bytes epilogue
if (mode == "bytes" ) 
	if(bytes.to_i < 1024) 
		size = "bytes"
		num = bytes
	elsif(bytes.to_i < 1048576) 
		size = "KB"
		num = bytes/1024
	elsif(bytes.to_i < 1073741824) 
		size = "MB"
		num = bytes/1048576
	else 
		size = "GB"	
		num = bytes/1073741824
	end
	puts "#{num} #{size}"
end

#time epilogue
if (mode == "time") 
		for i in 00..23
			if(i < 10) then i ="0"+i.to_s end
			puts i.to_s+" "+h[i.to_s].to_s
		end
	end

#popularity epilogue	
if(mode  == "popularity")
	g = h.sort { |a,b| b[1] <=> a[1]}
	
	if (h.size < 10) then n = h.size - 1
	else n = 9 end
	
	for b in 0..n
		puts g[b][1].to_s+' '+g[b][0]
	end
end

#requests epilogue
if(mode == "requests")
	h.keys.each do |ip| 
		r.push(ip.split(" ")[0].split("\.").push(h[ip],b[ip]))
	end
	
	r.sort!{ |x,y|
		t = x[0].to_i <=> y[0].to_i
		if t == 0 then
			t = x[1].to_i <=> y[1].to_i
			if t == 0 then
				t = x[2].to_i <=> y[2].to_i
				if t == 0 then
					t = x[3].to_i <=> y[3].to_i
				end
			end
		end
		t
	}	
	
	r.each do |a|
		puts a[0]+'.'+a[1]+'.'+a[2]+'.'+a[3]+' '+a[4].to_s+' '+a[5].to_s
	end
end	