# documentation on this file


# Stream provides an API for moving throughout the source file, which is currently hardcoded to "test.q"

# there are lots of classes called ____Parser
# each of these has a method parseFrom :: Stream -> hackety intermediate parse object
# 	this method leaves the stream right after the characters successfully parsed, or (currently) wherever it wants on fail
# each of these has a method execute :: hackety intermediate parse object -> ExecutionContext -> ()
#	this method inspects the intermediate parse object and then mutates the ExecutionContext

# it looks like parseFrom could be called from execute and execute could :: string -> bool

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

class CharParser
	def initialize char
		@char = char
	end
	
	def parseFrom stream
		char = stream.next
		if char == @char
			{ :success => true }
		else
			stream.prev
			{ :success => false }
		end
	end
end

class EachCharRegexpParser
	def initialize regexp
		@regexp = regexp
	end
	
	def parseFrom stream
		@parsed = ""
		char = stream.next
		
		while char =~ @regexp
			@parsed << char
			char = stream.next
		end
		
		{
			:success => @parsed.size > 0,
			:result => @parsed
		}
	end
end

class StringParser
	def parseFrom stream
		sequence_parser = SequenceParser.new [(CharParser.new '"')]
		
		sequence_parser.parseFrom stream
	end
end

class ManyParser
	def initialize &constructor
		@new_parser = proc do
			yield
		end
		@parsers = [@new_parser.call]
	end
	
	def parseFrom stream
		i = stream.i
		result = nil
		results = []
		begin
			i = stream.i
			
			parser = @new_parser.call
			@parsers << parser
			result = parser.parseFrom stream
			results << result
		end while result && result[:success]
		
		stream.i = i
		
		# always succeeds
		@results = {
			:success => true,
			:results => results
		}
	end
	
	def execute execution_context
		
		@parsers.each do |parser|
			parser.execute execution_context
		end
	end
end

class MaybeParser
	def initializes parser
		@parser = parser
	end
	
	def parseFrom stream
		@result = @parser.parseFrom stream
		
		if !result[:success]
			return { :success => true }
		end
		
		@result
	end
	
	def execute execution_context
		if @result[:success]
			@result.execute execution_context
		end
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
				@results = {
					:success => false,
					:results => results
				}
				
				return @results
			end
		end
		@results = {
			:success => true,
			:results => results
		}
	end
	
	def execute execution_context
		(0...@parsers.size).each do |i|
			@parsers[i].execute execution_context
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
		
		@results = {
			:success => true,
			:results => results
		}
		
	end
	
	def execute execution_context
		@parsers.zip(@results[:results]).each do |parser, result|
			parser.execute execution_context if result[:success]
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
			
			@result = {
				:success => true,
				:result => parsed
			}
		else
			{ :success => false }
		end
	end
	
	def execute execution_context
		execution_context[:current_symbol] = @result[:result]
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
		char = stream.next
		
		while char =~ /\s/
			char = stream.next
			parsed << char
		end
		
		stream.prev
		
		{
			:success => parsed.size > 0,
			:result => parsed
		}
	end
	
	def execute execution_context
	end
end

class TermParser
	def initialize
		@branch_parser = BranchParser.new [
			SymbolParser.new,
			StringParser.new
		]
	end
	
	def parseFrom stream
		@result = @branch_parser.parseFrom stream
	end
	
	def execute execution_context
		@result = @branch_parser.execute execution_context
	end
end

class InvokeParser
	def initialize
		@sequence_parser = SequenceParser.new [
			(SymbolParser.new),
			(ManyParser.new { SequenceParser.new [WhitespaceParser.new, TermParser.new] })
		]
	end
	
	def parseFrom stream
		@result = @sequence_parser.parseFrom stream
	end
	
	def execute execution_context
		symbol_parser_result = @result[:results][0]
		method_name = symbol_parser_result[:result]
		
		method_def = execution_context[:funcs].first { |method_def| method_def[:name] == method_name }
		
		many_parser_results = @result[:results][1][:results]
		many_parser_results.map do |whitespace_and_term|
			whitespace = whitespace_and_term[:results][0]
			term = whitespace_and_term[:results][1]
			
			@sequence_parser.execute execution_context
		end
	end
end

class ImportParser
	def parseFrom stream
		
		sequence_parser = SequenceParser.new [
			(WordParser.new "import"),
			(WhitespaceParser.new),
			(SymbolParser.new)
		]
		
		@parse_result = sequence_parser.parseFrom stream
	end
	
	def execute execution_context
		if @parse_result[:results][2][:result] == "IO"
			execution_context[:funcs] << {
				:name => "show",
				:execute => proc do |entity|
				
					entity.as(execution_context[:types]["Showable"])
				end
			}
		end
	end
end

class StatementParser
	def initialize
		@branch_parser = BranchParser.new [ImportParser.new, InvokeParser.new]
		@semicolon_parser = CharParser.new ';'
	end

	def parseFrom stream
		results = [(@branch_parser.parseFrom stream), (@semicolon_parser.parseFrom stream)]
		
		@result = {
			:success => results[0][:success] && results[1][:success],
			:results => results
		}
		
		@result
	end
	
	def execute execution_context
		@branch_parser.execute execution_context
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
		statement_parser.execute @execution_context
		
		puts @execution_context
	end
end

File.open "test.q" do |file|
	contents = file.read
	stream = Stream.new contents
	
	executor = Executor.new
	executor.execute stream
end