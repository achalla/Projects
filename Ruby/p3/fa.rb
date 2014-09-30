#!/usr/bin/ruby -w

class FiniteAutomaton
    @@nextID = 0	# shared across all states
    attr_reader:state, :start, :final, :alphabet, :transition, :state_subset, :marked, :e_marked

    #---------------------------------------------------------------
    # Constructor for the FA
    def initialize
        @start = nil 		# start state 
        @state = { } 		# all states
        @final = { } 		# final states
        @transition = {}	# transitions
        @alphabet = [ ] 	# symbols on transitions
		@e_close = Hash.new {|h,k| h[k] = [] }	
							#epsilon closure states
		@state_subset = {}	#for nfa->dfa
		@marked	= Hash.new(false)
							#for nfa->dfa
		@move = Hash.new {|h,k| h[k] = Hash.new {|a,b| a[b] = [] }	 }
		@e_marked = []
    end

	#attr_accessor :start, :state, :final, :transition, :alphabet, :e_closure
	
    #---------------------------------------------------------------
    # Return number of states
    def num_states
        @state.size
    end

    #---------------------------------------------------------------
    # Creates a new state 
    def new_state to_dfa = nil
        newID = @@nextID
        @@nextID += 1
        @state[newID] = true
        @transition[newID] = {}
		if(to_dfa) 
			if(to_dfa.kind_of?(Array))
				@state_subset[newID] = to_dfa 
			else
				@state_subset[newID] = [to_dfa]
			end
		end
        newID 
    end

    #---------------------------------------------------------------
    # Creates a new state
    def add_state(v)
		if !v then return end
		if !v.kind_of?(Array)
			unless has_state?(v)
				@state[v] = true
				@transition[v] = {}
			end
		else 
			v.each { |a|
				unless has_state?(a)
					@state[a] = true
					@transition[a] = {}
				end
			}
		end
    end

    #---------------------------------------------------------------
    # Returns true if the state exists
    def has_state?(v)
        @state[v]
    end

    #---------------------------------------------------------------
    # Set (or reset) the start state
    def set_start(v)
        add_state(v)
        @start = v
    end

    #---------------------------------------------------------------
    # Set (or reset) a final state
    def set_final(v, final = true)
        add_state(v)
        if final
            @final[v] = true
        else
            @final.delete(v)
        end
    end

    #---------------------------------------------------------------
    # Returns true if the state is final
    def is_final?(v)
        @final[v]
    end

    #---------------------------------------------------------------
    # Creates a new transition from v1 to v2 with symbol x
    # Any previous transition from v1 with symbol x is removed
    def add_transition(v1, v2, x)
		if !v1 || !v2 then return end
        add_state(v1)
		add_state(v2)
		if x == "E" then x == "" end
		if !v2 
			#puts "this is nil" 
			return
		end
		if a = @transition[v1][x]
			if !a.kind_of?(Array) 
				@transition[v1][x] = [a,v2]
			else
				a.push(v2)
			end
		else 
			@transition[v1][x] = v2
			#puts "trans[#{v1}] #{@transition[v1]}"
			#puts "inspect state: #{state.inspect}"
		end
    end

    #---------------------------------------------------------------
    # Get the destination state from v1 with symbol x
    # Returns nil if non-existent
    def get_transition(v1,x)
        if has_state?(v1)
            @transition[v1][x]
        else
            nil
        end
    end

    #---------------------------------------------------------------
    # Returns true if the dfa accepts the given string
    def accept?(s, current = @start)
        if s == ""
            is_final?(current)
        else
            dest = get_transition(current,s[0,1])
            if dest == nil
                false
            else
                accept?(s[1..-1], dest)
            end
        end
    end

    #---------------------------------------------------------------
    # Prints FA 
    def pretty_print
        print "% Start "
	puts @start

        # Final states in sorted order
	print "% Final {"
	@final.keys.sort.each { |x| print " #{x}" }
	puts " }" 

        # States in sorted order
	print "% States {"
	@state.keys.sort.each { |x| print " #{x}" }
	puts " }" 

        # Alphabet in alphabetical order
        print "% Alphabet {"
	@alphabet.sort.uniq.each { |x| print " #{x}" }
	puts " }" 

        # Transitions in lexicographic order
        puts "% Transitions {"
	@transition.keys.sort.each { |v1| 
            @transition[v1].keys.sort.each { |x| 
			v2 = get_transition(v1,x)
			#puts "THIS IS GET TRANS:"
			#puts "#{get_transition(v1,x)}"
				if v2.kind_of?(Array)
					v2.each do |b|
						#puts "v2: #{v2}"
						puts "%  (#{v1} #{x} #{b})" 
					end
				else
					#puts "v2: #{v2}"
					puts "%  (#{v1} #{x} #{v2})" 
				end
            }
        }
	puts "% }" 
    end
        
    #---------------------------------------------------------------
    # Prints FA statistics
    def print_stats
        puts "FiniteAutomaton"
        puts "  #{@state.size} states"
        puts "  #{@final.size} final states" 
        puts "  #{get_trans_size.to_s} transitions"
		t_stats = Hash[get_trans_stats.sort]
		t_stats.each do |a,b|
			puts "    #{b} states with #{a} transitions"
		end
    end
	
	#---------------------------------------------------------------
    # Helper for FA stats
	
	def get_trans_size
		count = 0
		@transition.each do |c,d|
			#puts "c: #{c}, d: #{d}" 
			if d.empty? 
				#puts "true"
			else
				
				d.values.each do |f|
					if f.kind_of?(Array)
						f.each do |g|
							count += 1
							#puts "count: #{count}"
						end
					else
						count += 1
						#puts "count: #{count}"
					end
				end				
			end
		end
		#puts "FINAL COUNT: #{count}"
		count
	end
	
	#---------------------------------------------------------------
    # Helper for FA stats
	
	def get_trans_stats
		sorter = Hash.new(0)
		state_sorter = Hash.new(0)
		total = 0
		@transition.each do |a,b|
			#puts "a: #{a} b: #{b}"
			if b.values[0].kind_of?(Array) 
				sorter[a] += b.values[0].size
				#puts "b inspect
			elsif b.empty?
				#puts "true"
			else sorter[a] += 1
			end
			#puts "a: #{a}, sorter[#{a}]: #{sorter[a]}"
		end
		
		sorter.values.each do |c|
			state_sorter[c] += 1
		end
		
		#puts "ss inspect: #{state_sorter.inspect}"
		#@state.each do |s|	
		#	state_sorter[s] => state_sorter[s] 
		#end
		state_sorter.each do |c,d|	
			total += d
		end
		
		zeroes = (@state.size - total)
		if zeroes != 0 then state_sorter[0] = zeroes end
		
		state_sorter
	end
	
	
    #---------------------------------------------------------------
    # accepts just symbol ("" = epsilon)
    def symbol! sym
        initialize
        s0 = new_state
        s1 = new_state
        set_start(s0)
        set_final(s1, true)
        add_transition(s0, s1, sym)
        if (sym != "") && (!@alphabet.include? sym)
            @alphabet.push sym
        end
    end

    #---------------------------------------------------------------
    # accept strings accepted by self, followed by strings accepted by newFA
    
	def concat! newFA
		#puts "concat!".upcase
		if (@final.length != 1)
			puts "UNEXPECTED BEHAVIOR 1!" 
		else 
			#puts "newfa trans #{newFA.transition.inspect}"
			#puts "okay so they're correct coming in"
			old_final = @final.keys[0]
			#puts "final keys [0]: #{old_final}"
			#puts "and: #{newFA.start}"
			#puts "inspect final: #{@final.inspect}"
			#puts "what"
			add_transition(old_final,newFA.start,"")
			newFA.transition.each do |k,v|
				#@transition[k] = v
				#puts "@state inspect: #{state.inspect}"
				add_transition(k,v.values[0],v.keys[0])
			end
			set_final(old_final, false)
			#puts "inspect final 2: #{@final.inspect}"
			#puts "inspect newFA's final : #{newFA.final}"
			new_final = newFA.final.keys[0]
			set_final(new_final, true)
			#puts "inspect final 4: #{@final.inspect}"
			#puts "oh"
			newFA.alphabet.each do |alp|
				@alphabet.push(alp)
			end
		end
	end

    #---------------------------------------------------------------
    # accept strings accepted by either self or newFA
    def union! newFA
		#puts "union!".upcase
		new_start = new_state
		add_transition(new_start, @start, "")
		add_transition(new_start, newFA.start, "")
		#puts "start #{@start} newfa start #{newFA.start}"
		@start = new_start
		
		old_final = @final.keys[0]
		old_final2 = newFA.final.keys[0]
		new_final = new_state
		add_transition(old_final,new_final,"")
		#puts "oldfinal2: #{old_final2}"
		add_transition(old_final2,new_final,"")
		
		newFA.transition.each do |k,v|
			#puts "k: #{k}, V: #{v.values[0]}, #{v.keys[0]}"
			#puts "#{v.values[0].nil?}"
			add_transition(k,v.values[0],v.keys[0])
		end
		
		set_final(old_final, false)
		set_final(old_final2, false)
		set_final(new_final)	


		newFA.alphabet.each do |alp|
			@alphabet.push(alp)
		end		
    end

    #---------------------------------------------------------------
    # accept any sequence of 0 or more strings accepted by self
    def closure! 
		#puts "CLOSURE!"
		new_start = new_state
		add_transition(new_start, @start, "")
		@start = new_start
			
		old_final = @final.keys[0]
		new_final = new_state
		add_transition(old_final,new_final,"")
		set_final(old_final, false)
		set_final(new_final)
		
		add_transition(new_start,new_final,"")
		add_transition(new_final,new_start,"")
	
    end

    #---------------------------------------------------------------
    # returns DFA that accepts only strings accepted by self 
    def to_dfa
       # create a new one, or modify the current one in place,
        # and return it
		#R = state_subset, R[r] = e_clo or whatever
		
		
		populate_move
		fan = FiniteAutomaton.new
		#puts "move: #{@move.inspect}"
		new_start = new_state
		fan.state_subset[new_start] = e_main(@start)	#@move[@start][""]
		puts "state_subset.inspect: #{fan.state_subset.inspect}"
		
		#puts "TEST #{@move[3][5]}"
		
		fan.add_state(new_start)
		fan.set_start(new_start)
		rec_dfa_helper(fan,new_start)
		puts "fan subst inspect: #{fan.state_subset.inspect}"
		puts "\n\n\n\n\n"
		
		self
    end
	
	
	#this function recursively follows every transition on a state and maps it to 
	#a state of state subsets, creating a new one if need be.
	def rec_dfa_helper (fan, state)
		ret = nil
		puts "BEGIN"
		@transition.each do |k,v|
			if k == state # we're getting all the transitions from this state
				puts "here we go: state #{k} #{fan.state_subset[state].inspect}"
				reachables = Hash.new {|i,j| i[j] = [] }	
				@alphabet.each do |a|
					fan.state_subset[state].each do |substate|
					
						if(!@move[substate][a].empty?)
							@move[substate][a].each do |reachable|
								reachables[a].push(reachable)
							end
						end
					
					
						#if(!e_main(substate).empty?)	
						#	e_main(substate).each do |reachable|
						#		temp.push(reachable)
						#	end
						#end	
					end
					reachables[a].sort!.uniq!
					ret = reachables[a]
					
					#now we assign it a state AND A TRANSITION
					if (fan.state_subset.values.include? reachables[a])
						puts "hi i'm here"
						n_state = fan.state_subset.values.index(reachables[a])
						#reachables[a].each do |z|
							fan.add_transition(k,n_state,a)
						#end		
						rec_dfa_helper(fan, n_state)
					else
						puts "nope, add me!!"
						n_state = fan.new_state
						fan.add_state(n_state)
						fan.state_subset[n_state] = reachables[a]
						fan.add_transition(k,n_state,a)
						temp = []
						reachables[a].each do |z|		
							if(y = rec_dfa_helper(fan, z)) 
								if(y.kind_of?(Array))
									y.each do |t1|
										temp.push(t1)
									end
								elsif(y)
									temp.push(y)
								end
							end
						end
						puts "temp: #{temp.inspect}"
					end		
				
				
				end
				puts "there we went: #{fan.state_subset[state].inspect}"
				puts "state: #{state}, reachables: #{reachables.inspect}"
			end
		end		
		puts "TRANSITION: #{fan.transition.inspect}"
		#fan.transition.each do |k,v|
		#	v.each do |trans, rec|
		#		if state_subset.values.include? rec
		#	end
		#end
		ret
	end
	
	
	
	
	
	
	
	def populate_move
		@transition.each do |k,v|
			v.each do |trans, rec|
				if(rec.kind_of?(Array))
					rec.each do |r|
						@move[k][trans].push(r)
						@e_marked = []
						
							e_main(r).each do |e|
								@move[k][trans].push(e)
							end
							
					end
				else
					@move[k][trans].push(rec)
						@e_marked = []
						e_main(rec).each do |e|
							@move[k][trans].push(e)
						end
						
				end	
			end
		end
	
	#sort
	@transition.each do |k,v|
		v.each do |trans, rec|
			@move[k][trans].sort!.uniq!
		end
	end
		
		@move
	end
	
	#---------------------------------------------------------------
    # helper method for to_dfa
	
	def e_main state
		@e_marked = []
		e_closure(state)
	end
	
	
	#appears to work?
	def e_closure state
		#puts "BEGIN #{@e_close[state].inspect}, state: #{state}"
		@e_close[state] = [state]
		#puts "INITIALIZE #{@e_close[state].inspect}"
		@e_marked.push(state)
		#puts "inspect @e_marked: #{@e_marked.inspect}"
		@transition.each do |k,v|
			if(k == state)
				v.each do |trans, rec|
		#			puts "state: #{k}, trans: #{trans}, rec: #{rec}"
					if(trans == "")
						#if !@e_marked.include?(rec)
							if(!rec.kind_of?(Array))
								if !@e_marked.include?(rec)
		#							puts "TWO #{rec}"
									if(e_closure(rec).kind_of?(Array))
										e_closure(rec).each do |r|
		#									puts "ONE #{rec}"
											@e_close[state].push(r)
										end
									elsif(e_closure(rec))
		#								puts "ZERO #{rec}"
										@e_close[state].push(rec)
									end
								end								
							else
		#						puts "THREE #{rec}"
								rec.each do |r1|
									if !@e_marked.include?(r1)
										e_r1 = e_closure(r1)
		#								puts "ecl on #{r1}: #{e_r1}"
										if(e_r1.kind_of?(Array))
		#									puts "FOUR #{e_r1}"
											e_r1.each do |r|
												@e_close[state].push(r)
		#										puts "ecl insp #{@e_close[state].inspect}"
											end
										elsif(e_r1)
		#									puts "FIVE #{e_r1}"
											@e_close[state].push(r1)
										end
									end
								end
							end
						#end
					end
				end
			end
		end
		#@e_close[state].sort!
		#puts "RETURN #{@e_close[state].inspect}, state: #{state}"
		@e_marked.sort!.uniq!
		#puts "RETURN #{@e_marked.inspect}"
		@e_marked
	end



    #---------------------------------------------------------------
    # returns a DFA that accepts only strings not accepted by self, 
    # and rejects all strings previously accepted by self
    def complement!
        # create a new one, or modify the current one in place,
        # and return it
        FiniteAutomaton.new     
    end

    #---------------------------------------------------------------
    # return all strings accepted by FA with length <= strLen
    def gen_str strLen
	sortedAlphabet = @alphabet.sort
        resultStrs = [ ] 
        testStrings = [ ]
        testStrings[0] = [] 
        testStrings[0].push ""
        1.upto(strLen.to_i) { |x|
            testStrings[x] = []
            testStrings[x-1].each { |s|
                sortedAlphabet.each { |c|
                    testStrings[x].push s+c
                }
            }
        }
        testStrings.flatten.each { |s|
            resultStrs.push s if accept? s
        }
        result = ""
        resultStrs.each { |x| result.concat '"'+x+'" ' }
        result
    end

end

#---------------------------------------------------------------
# read standard input and interpret as a stack machine

def interpreter file
   dfaStack = [ ] 
   loop do
       line = file.gets
       if line == nil then break end
       words = line.scan(/\S+/)
       words.each{ |word|
           case word
               when /DONE/
                   return
               when /SIZE/
                   f = dfaStack.last
                   puts f.num_states
               when /PRINT/
                   f = dfaStack.last
                   f.pretty_print
               when /STAT/
                   f = dfaStack.last
                   f.print_stats
               when /DFA/
                   f = dfaStack.pop
                   f2 = f.to_dfa
                   dfaStack.push f2
               when /COMPLEMENT/
                   f = dfaStack.pop
                   f2 = f.complement!
                   dfaStack.push f2
               when /GENSTR([0-9]+)/
                   f = dfaStack.last
                   puts f.gen_str($1)
               when /"([a-z]*)"/
                   f = dfaStack.last
                   str = $1
                   if f.accept?(str)
                       puts "Accept #{str}"
                   else
                       puts "Reject #{str}"
                   end
               when /([a-zE])/
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f = FiniteAutomaton.new
                   sym = $1
                   sym="" if $1=="E"
                   f.symbol!(sym)
                   dfaStack.push f
               when /\*/
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f = dfaStack.pop
                   f.closure!
                   dfaStack.push f
               when /\|/
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f1 = dfaStack.pop
                   f2 = dfaStack.pop
                   f2.union!(f1)
                   dfaStack.push f2
               when /\./
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f1 = dfaStack.pop
                   f2 = dfaStack.pop
                   f2.concat!(f1)
                   dfaStack.push f2
               else
                   puts "Ignoring #{word}"
           end
        }
   end
end

#---------------------------------------------------------------
# main( )

if false			# just debugging messages
    f = FiniteAutomaton.new
    f.set_start(1)
    f.set_final(2)
    f.set_final(3)
    f.add_transition(1,2,"a")   # need to keep this for NFA
    f.add_transition(1,3,"a")  
    f.prettyPrint
end

if ARGV.length > 0 then
  file = open(ARGV[0])
else
  file = STDIN
end

interpreter file  # type "DONE" to exit

