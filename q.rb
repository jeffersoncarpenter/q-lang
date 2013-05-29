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
end

class BranchParser
	def initialize parsers
		@parsers = parsers
	end
	
	def parseFrom stream
		@parsers.each do |parser|
			result = parser.parseFrom stream
			return {
				:success => true,
				:type => parser.class,
				:result => result
			} if result[:success]
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
		
		while stream.next =~ /\s/
			success = true
		end
		
		stream.prev
		
		{
			:success => success
		}
	end
end

class ImportParser
	def parseFrom stream
	
		sequence_parser = SequenceParser.new [
			(WordParser.new "import"),
			(WhitespaceParser.new),
			(SymbolParser.new),
			(SemicolonParser.new)
		]
		
		sequence_parser.parseFrom stream
	end
	
	def execute parse_result execution_context
		if parse_result[:results][2] == "IO"
			execution_context
		end
	end
end

class StatementExecutor
	def execute stream
		parser = BranchParser.new [ImportParser.new]
		
		result = parser.parseFrom stream
		
		if result[:success]
		end
	end
end

class Executor
	def execute stream
		statement_executor = StatementExecutor.new
		result = statement_executor.execute stream
	end
end

File.open "test.q" do |file|
	contents = file.read
	stream = Stream.new contents
	
	executor = Executor.new
	result = executor.execute stream
	puts result
end