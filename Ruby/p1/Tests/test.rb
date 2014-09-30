
class Cat
	def initialize()
		@name = "Fluffy"
		@age = 7
		@male = false
	end
	
	def to_s
		@name
	end

	def print_info
		puts "#{@name} says 'Meow!'"
		print @male ? "He " : "She " 
		puts "is #{@age} years old!"
	end
	
	attr_accessor :name, :age, :male
end