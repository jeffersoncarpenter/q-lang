# documentation on this file


# Stream provides an API for moving throughout the source file, which is currently hardcoded to "test.q"

# there are lots of classes called ____Parser
# each of these has a method parseFrom :: Stream -> hackety intermediate parse object
# 	this method leaves the stream right after the characters successfully parsed, or (currently) wherever it wants on fail
# each of these has a method execute :: hackety intermediate parse object -> ExecutionContext -> ExecutionContext
#	this method inspects the intermediate parse object and the execution context, and returns a modified execution context

# the hackety intermediate parse objects are hashes with the following structure
# {
# 	:success => whether the parser found anything,
#	:result(s) => object that contains the essential matched features from the file
# }



class Stream
	def initialize enum
		@i = -1
		@enum = enum
	end
	
	def clone
		stream = CharStream.new @enum
		stream.i = self.i
	end
	
	def next
		@i = @i + 1
		if @i < @enum.size
			return @enum[@i]
		end
	end
	
	def prev
		@i -= 1
		if @i >= 0
			return @enum[i]
		end
	end
	
	def i
		@i
	end
	
	def i= i
		@i = i
	end
end

class RegexpParser
	def initialize regexp
		@regexp = regexp
	end
end

class ManyParser
	def initialize parser
		@parser = parser
	end
	
	def parseFrom stream
		i = stream.i
		result = nil
		results = []
		begin
			i = stream.i
			
			result = @parser.parseFrom stream
			
			results << result
		end while result && result[:success]
		
		stream.i = i
		
		# always succeeds
		{
			:success => true,
			:results => results
		}
	end
end

class SequenceParser
	def initialize parsers
		@parsers = parsers
	end
	
	def parseFrom stream
		results = []
		@parsers.each do |parser|
			result = parser.parseFrom stream
			results << result
			if result[:success] == false
				return {
					:success => false,
					:results => results
				}
			end
		end
		{
			:success => true,
			:results => results
		}
	end
	
	def execute results, execution_context
		[1...@parsers.size].each do |i|
			@parsers[i].execute results[i], execution_context
		end
	end
end

class BranchParser
	def initialize parsers
		@parsers = parsers
	end
	
	def parseFrom stream
		results = []
		
		i = stream.i
		@parsers.each do |parser|
			stream.i = i
			result = (parser.parseFrom stream)
			results << result
		end
		
		{
			:success => true,
			:results => results
		}
	end
	
	def execute parse_results, execution_context
		[0...@parsers.size].each do |i|
			puts parse_results[:results][i]
			puts parse_results[:results][i].class
			@parsers[i].execute parse_results[:results][i], execution_context if parse_results[:results][i][:success]
		end
	end
end

class SemicolonParser
	def parseFrom stream
		if stream.next == ';'
			{ :success => true }
		else
			stream.prev
			{ :success => false }
		end
	end
end

class SymbolParser
	def parseFrom stream
	
		parsed = ""
		
		symbolFirstCharacterRegex = /[a-zA-Z_]/
		symbolInteriorRegex = /[a-zA-Z0-9_!?]/
		
		next_char = stream.next
		if next_char =~ symbolFirstCharacterRegex
			
			until !(next_char =~ symbolInteriorRegex)
				parsed << next_char
				next_char = stream.next
			end
			
			stream.prev
			
			{
				:success => true,
				:match => parsed
			}
		else
			{ :success => false }
		end
	end
end

class WordParser
	def initialize word
		@word = word
	end
	
	def parseFrom stream
	
		parsed = ""
		
		while @word.index(parsed) == 0 do
			parsed += stream.next
		end
		
		parsed.chop!
		stream.prev
		
		{
			:success => @word == parsed
		}
	end
end

class WhitespaceParser

	def parseFrom stream
	
		success = false
		parsed = ""
		char = ""
		
		begin
			char = stream.next
			parsed << char
		end while char =~ /\s/
		
		stream.prev
		
		{
			:success => parsed.size > 0,
			:result => parsed
		}
	end
end

class InvokeParser
	def initialize
		@sequence_parser = SequenceParser.new [
			(SymbolParser.new),
			(ManyParser.new (SequenceParser.new [WhitespaceParser.new, SymbolParser.new]))
		]
	end
	
	def parseFrom stream
		@sequence_parser.parseFrom stream
	end
end

class ImportParser
	def parseFrom stream
		
		sequence_parser = SequenceParser.new [
			(WordParser.new "import"),
			(WhitespaceParser.new),
			(SymbolParser.new)
		]
		
		sequence_parser.parseFrom stream
	end
	
	def execute parse_result, execution_context
		if parse_result[:result][:results][2][:match] == "IO"
			execution_context[:funcs] << {
				:name => "show",
				:execute => proc do |arg|
					arg.as("Showable")
				end
			}
		end
	end
end

class StatementParser
	def initialize
		@branch_parser = BranchParser.new [ImportParser.new, InvokeParser.new]
		@semicolon_parser = SemicolonParser.new
	end

	def parseFrom stream
		results = [(@branch_parser.parseFrom stream), (@semicolon_parser.parseFrom stream)]
		{
			:success => results[0][:success] && results[1][:success],
			:results => results
		}
	end
	
	def execute parse_result, execution_context
		@branch_parser.execute parse_result[:results][0], execution_context
	end
end

class Executor
	def initialize
		@execution_context = {
			:funcs => [],
			:tasks => [],
			:values => [],
			:types => [],
			:interfaces => []
		}
	end
	
	def execute stream
		statement_parser = StatementParser.new
		parse_result = statement_parser.parseFrom stream
		statement_parser.execute parse_result, @execution_context
		
		puts @execution_context
	end
end

File.open "test.q" do |file|
	contents = file.read
	stream = Stream.new contents
	
	executor = Executor.new
	executor.execute stream
end