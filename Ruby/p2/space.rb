##########################################################################
### CMSC330 Project 2: Multi-threaded Space Simulation                 ###
### Source code: space.rb                                              ###
### Description: Multi-threaded Ruby program simulating space travel   ###
### Student Name: Anu Challa                                           ###
##########################################################################

require "monitor"

Thread.abort_on_exception = true   # to avoid hiding errors in threads 

#------------------------------------
# Global Variables
        
$headerPorts = "=== Starports ==="
$headerShips = "=== Starships ==="
$headerTraveler = "=== Travelers ==="
$headerOutput = "=== Output ==="

$simOut = []            # simulation output

$starport = []
$starship = []
$traveler = []

#----------------------------------------------------------------
# Starport 
#----------------------------------------------------------------

class Starport
    def initialize (name,size)
        @name = name
        @size = size
        @ships = []
        @travelers = []
		@lock = Monitor.new
		@cap_cond = @lock.new_cond
		@shipAvail = false
    end
    
    def to_s
        @name
    end

    def size
        @size
    end
	
    def arrive(person)
        @travelers.push(person)
    end
	
	attr_accessor :name, :size, :ships, :travelers, :lock, :cap_cond, :shipAvail
end

#------------------------------------------------------------------
# find_name(name) - find port based on name

def find_name(arr, name)
    arr.each { |p| return p if (p.to_s == name) }
    puts "Error: find_name cannot find #{name}"
        $stdout.flush
end

#------------------------------------------------------------------
# next_port(c) - find port after current port, wrapping around

def next_port(current_port)
    port_idx = $starport.index(current_port)
    if !port_idx
        puts "Error: next_port missing #{current_port}"
        $stdout.flush
        return  $starport.first
    end
    port_idx += 1
    port_idx = 0 if (port_idx >= $starport.length)
    $starport[port_idx]
end

#----------------------------------------------------------------
# Starship 
#----------------------------------------------------------------

class Starship 
    def initialize (name,size)
        @name = name
		@size = size
        @passengers = []
		@docked = false #toggle
		@portq = []
		@lock = Monitor.new	
		@cap_cond = lock.new_cond
		@curr_planet = nil
    end
	
    def size
        @size
    end
	    
    def to_s
        @name
    end
	
	attr_accessor :name, :size, :passengers, :docked, :portq, :lock, :cap_cond, :curr_planet
end         


#----------------------------------------------------------------
# Traveler 
#----------------------------------------------------------------

class Traveler
    def initialize(name, itinerary)
        @name = name
        @itinerary = itinerary
		@checklist = itinerary
		@last = itinerary[0]
		@onboard = false
		@curr_ship
    end

    def to_s
        @name
    end
	
    def itinerary
        @itinerary
    end
	
	attr_accessor :name, :itinerary, :checklist, :last, :onboard, :curr_ship
end

#------------------------------------------------------------------
# read command line and decide on display(), verify() or \()

def readParams(fname)
    begin
        f = File.open(fname)
    rescue Exception => e
        puts e
        $stdout.flush
        exit(1)
    end

    section = nil
    f.each_line{|line|

        line.chomp!
        line.strip!
        if line == "" || line =~ /^%/
            # skip blank lines & lines beginning with %

        elsif line == $headerPorts || line == $headerShips ||
        line == $headerTraveler || line == $headerOutput
            section = line

        elsif section == $headerPorts
            parts = line.split(' ')
            name = parts[0]
            size = parts[1].to_i
            $starport.push(Starport.new(name,size))
			#$portq.push(Starport.new(name,size))
                
        elsif section == $headerShips
            parts = line.split(' ')
            name = parts[0]
			size = parts[1].to_i
            $starship.push(Starship.new(name,size))

        elsif section == $headerTraveler
            parts = line.split(' ')
            name = parts.shift
            itinerary = []
			checklist = []
            parts.each { |p| 
			itinerary.push(find_name($starport,p))
			checklist.push(find_name($starport,p))}
            person = Traveler.new(name,itinerary)
            $traveler.push(person)
            find_name($starport,parts.first).arrive(person)

        elsif section == $headerOutput
            $simOut.push(line)
			#puts "line: #{line}"

        else
            puts "ERROR: simFile format error at #{line}"
            $stdout.flush
            exit(1)
        end
    }
	$starship.each do |ship| ship.portq = $starport.clone end
end

#------------------------------------------------------------------
# 

def printParams()
    
    puts $headerPorts
    $starport.each { |s| puts "#{s} #{s.size}" }
    
    puts $headerShips 
    $starship.each { |s| puts "#{s} #{s.size}" }
    
    puts $headerTraveler 
    $traveler.each { |p| print "#{p} "
                               p.itinerary.each { |s| print "#{s} " } 
                               puts }

    puts $headerOutput
    $stdout.flush
end

#----------------------------------------------------------------
# Simulation Display
#----------------------------------------------------------------

def array_to_s(arr)
    out = []
    arr.each { |p| out.push(p.to_s) }
    out.sort!
    str = ""
    out.each { |p| str = str << p << " " }
    str
end

def pad_s_to_n(s, n)
    str = "" << s
    (n - str.length).times { str = str << " " }
    str
end

def ship_to_s(ship)
    str = pad_s_to_n(ship.to_s,12) << " " << array_to_s(ship.passengers)
    str
end

def display_state()
    puts "----------------------------------------"
    $starport.each { |port|
        puts "#{pad_s_to_n(port.to_s,13)} #{array_to_s(port.travelers)}"
        out = []
        port.ships.each { |ship| out.push("  " + (ship_to_s(ship))) }
        out.sort.each { |line| puts line }
    }
    puts "----------------------------------------"
end


#------------------------------------------------------------------
# display - print state of space simulation

def display()
    display_state()
    $simOut.each {|o|
        puts o
        if o =~ /(\w+) (docking at|departing from) (\w+)/
            ship = find_name($starship,$1); 
            action = $2;
            port = find_name($starport,$3); 
            if (action == "docking at")
                port.ships.push(ship)
            else
                port.ships.delete(ship)
            end
                
        elsif o =~ /(\w+) (board|depart)ing (\w+) at (\w+)/
            person = find_name($traveler,$1); 
            action = $2;
            ship = find_name($starship,$3); 
            port = find_name($starport,$4); 
            if (action == "board")
                ship.passengers.push(person)
                port.travelers.delete(person)
            else 
                ship.passengers.delete(person)
                port.travelers.push(person)
            end
        else
            puts "% ERROR Illegal output #{o}"
        end
        display_state()
    }
end

#------------------------------------------------------------------
# verify - check legality of simulation output

def verify
    validSim = true
    $simOut.each {|o|
        if o =~ /(\w+) (docking at|departing from) (\w+)/
            ship = find_name($starship,$1); 
            action = $2;
            port = find_name($starport,$3); 
            if (action == "docking at")
				port.ships.push(ship)
				if port.ships.size > port.size then validSim = false end
				#puts "#{port} ships: #{port.ships.inspect}"
				if (ship.docked == true) then validSim = false end
				ship.docked = true
				if(ship.portq[0] != port) then validSim = false end
				ship.portq.push(ship.portq.shift)
            else # departing
				port.ships.delete(ship)
				if (ship.docked == false) then validSim = false end
				ship.docked = false
            end
			
        elsif o =~ /(\w+) (board|depart)ing (\w+) at (\w+)/
            person = find_name($traveler,$1); 
            action = $2;
            ship = find_name($starship,$3); 
            port = find_name($starport,$4); 
			
            if (action == "board")
				ship.passengers.push(person)
				port.travelers.push(person)
				if ship.passengers.size > ship.size then validSim = false end
				if !ship.docked then validSim = false end
				if port != person.checklist[0] then validSim = false end
            else 
				ship.passengers.delete(person)
				port.travelers.delete(person)
				person.last = port
				if port == person.checklist[1] then person.checklist.shift 
				else validSim = false end
				if !ship.docked then validSim = false end
			end
        else
            puts "% ERROR Illegal output #{o}"
        end
    }
	
	$traveler.each do |p| if p.itinerary[-1] != p.last then validSim = false end end
    return validSim
end

#------------------------------------------------------------------
# simulate - perform multithreaded space simulation

def simulate()
	Thread.abort_on_exception = true
	$print = Monitor.new
	trav_threads = []
	ship_threads = []
	done = false
	
		
	$traveler.size.times do |index|
	#important: each passenger is always trying to go to 
	# the SECOND item on checklist, aka checklist[1]
	# (assumption is that they are on checklist[0]
		i = index
		trav_threads[i] = Thread.new{ 
			
			curr_trav = $traveler[i]
			curr_loc = curr_trav.checklist[0]
			curr_ship = nil
			
			target = curr_trav.checklist[1]
			while(target)
				#board
				curr_loc.lock.synchronize{
					while(curr_loc.travelers.include?(curr_trav))
						curr_loc.cap_cond.wait_until{!curr_loc.ships.empty? && has_space?(curr_loc.ships)}
						curr_loc.ships.each{ |s|
							if s.passengers.size < s.size
								$print.synchronize{
									puts "#{curr_trav} boarding #{s} at #{curr_loc}"
									$stdout.flush
								}
								s.passengers.push(curr_trav)
								curr_loc.travelers.delete(curr_trav)
								curr_ship = s
								break
							end
						}
					end
				}
				
				#disembark
				target.lock.synchronize{
					target.cap_cond.wait_until{target.ships.include? curr_ship}
					$print.synchronize{
						puts "#{curr_trav} departing #{curr_ship} at #{target}"
						$stdout.flush
					}
					curr_ship.passengers.delete(curr_trav)
					target.travelers.push(curr_trav)
					curr_trav.checklist.shift
					target = curr_trav.checklist[1]
					#iteration condition setup
					curr_loc = curr_trav.checklist[0]
					if(!target) 
						#puts "NO TARGET\n\n" 
						break
					end
					target = find_name($starport,target.to_s)
					curr_ship = nil
					#done = true ######################### should go after while
				}
			
				sleep 0.001	
				
			end
			#done = true ######################### should go after while
			
			#puts target
			#target.each do |a| puts a end
			#puts i
		}
    end
	
	$starship.size.times do |i|
		ship_threads[i] = Thread.new{
			#since ships start in space, the destination is
			# portq[0], not portq[1]. 
			# they will be at portq[-1]
			
			curr_ship = $starship[i]			
			target = curr_ship.portq[0]
			curr_loc = nil
			
			while(!done)
			
				#take off
				if(curr_loc)
					#puts "#{curr_loc}, ship: #{curr_ship}" 
					curr_loc.lock.synchronize{
						
						$print.synchronize{
							puts "#{curr_ship} departing from #{curr_loc}"
							$stdout.flush
						}
						curr_loc.ships.delete(curr_ship)
						curr_loc.cap_cond.broadcast
						#broadcast
					}
				end
				
				#touch down
				target.lock.synchronize{
				
					target.cap_cond.wait_until{target.ships.size < target.size} #wait until condition
						$print.synchronize{
							puts "#{curr_ship} docking at #{target}"
							$stdout.flush
						}
						target.ships.push(curr_ship)
						target.shipAvail = true
						target.cap_cond.broadcast
					
				}
				sleep 0.001
				
				###
						curr_ship.portq.push(curr_ship.portq.shift)
						target = curr_ship.portq[0]
						curr_loc = curr_ship.portq[-1]
						target = find_name($starport,target.to_s)
						#if(!target) then puts "WTF no target?" end ###
				
				
			end
		}
    end
			
	trav_threads.size.times do |i| trav_threads[i].join end
	#ship_threads.size.times do |i| ship_threads[i].join end
end

#------------------------------------------------------------------
#helper method

def has_space? (ships)
	if ships == nil then return false end
	ships.each{ |s|
		if s.passengers.size < s.size
			return s
		end
	}
	return nil
end

#------------------------------------------------------------------
# main - simulation driver

def main
    if ARGV.length != 2
        puts "Usage: ruby space.rb [simulate|verify|display] <simFileName>"
        exit(1)
    end
    
    # list command line parameters
    cmd = "% ruby space.rb "
    ARGV.each { |a| cmd << a << " " }
    puts cmd
    
    readParams(ARGV[1])
  
    if ARGV[0] == "verify"
        result = verify()
        if result
            puts "VALID"
        else
            puts "INVALID"
            exit(1)
        end

    elsif ARGV[0] == "simulate"
        printParams()
        simulate()

    elsif ARGV[0] == "display"
        display()

    else
        puts "Usage: space [simulate|verify|display] <simFileName>"
        exit(1)
    end
    exit(0)
end

main

