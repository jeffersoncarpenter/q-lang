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

class MultiParser
	def execute_all parse_results, execution_context
		[0...@parsers.size].each do |i|
			@parsers[i].execute parse_results[:results][i], execution_context
		end
	end
end

class ManyParser
	def initialize parser
		@parser = parser
	end
	
	def parseFrom stream
		i = 0
		result = nil
		results = []
		begin
			i = stream.i
			
			result = @parser.parseFrom stream
			
			if !result[:success]
				stream.i = i
				return results
			end
			
			results << result
		end while result && result[:success]
		
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
		i = stream.i
		results = []
		@parsers.each do |parser|
			result = parser.parseFrom stream
			results << result
			if result[:success] == false
				stream.i = i
				return results
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
		i = stream.i
		@parsers.each do |parser|
			stream.i = i
			result = parser.parseFrom stream
			return {
				:success => true,
				:type => parser.class,
				:result => result
			} if result[:success]
		end
	end
	
	def execute parse_results, execution_context
	
		
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
		
		while stream.next =~ /\s/
			success = true
		end
		
		stream.prev
		
		{
			:success => success
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
		puts "here"
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