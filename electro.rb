class Test
  def self.test
    yield
  end
end

class QType
  # @@implementations[implementent_type][implementing_type]
  @@implementations = {}

  def self.implementations
    @@implementations
  end

  def initialize
    QType.implementations[self.class] = {}
  end

  def self.implements? type
    puts QType.implementations
    self == type || QType.implementations[self].keys.index {|type| type.implements? type}
  end

  def as type
    self if self.class == type
    implementation = QType.implementations[self.class][type]
    if implementation
      implementation.call type
    end
  end

  def self.implement type, implementation
    QType.implementations[type] = implementations
  end
end

# returns an array of objects sorted by the constraint they match
# no repetitions in the returned array
def match constraints, objects
  # because this uses permutations, worst-case is O(n!)
  
  best_match = nil
  best_match_count = 0

  objects.permutation do |p|
    this_permutation_match_count = 0
    constraints.each do |constraint|
      p.each do |object|
        this_permutation_match_count += 1 if constraint.call object
      end
    end
    if this_permutation_match_count == objects.size
      return p
    elsif this_permutation_match_count > best_match_count
      best_match = p
      best_match_count =  this_permutation_match_count
    end
  end
  if !best_match
    puts "Unable to find match"
    puts "constraints:"
    puts constraints
    puts "objects:"
    puts objects
  end
  return best_match
end

class QMethod
  def initialize param_types, return_type, proc
    @param_types = param_types
    @return_type = return_type
    @proc = proc
  end

  def call args
    constraints = @param_types.map do |type|
      proc {|object| object.class.implements? type}
    end
    
    ordered_args = match constraints, args
    
    invoke ordered_args, @param_types.dup do |args_as_types|
      @proc.call args_as_types
    end
  end

  def invoke ordered_args, types, &block
    first_arg = ordered_args.shift
    first_type = types.shift

    first_arg.as first_type do |cast_arg|
      if ordered_args.size > 0
        invoke ordered_args, types do |results|
          results = [cast_arg].concat results
          yield results
        end
      else
        yield [cast_arg]
      end
    end
  end
end

class QString < QType
  implement String, proc {|qString| qString.as String }
  
  def initialize string
    @string = string
    super()
  end

  def as type, &block
    if type == String
      yield @string
    end

    super type do |cast_type|
      yield cast_type
    end
  end
end

class QArray < QType
  def initialize type
    @type = type
    @array = []
    super
  end

  def push object
    if object.class == @type
      @array << object
    end
  end

  def as type, &block
    if type == @type
      @array.each {|object| yield object }
    end
  end
end



# test case 1: QString type auto-casts to String

show = QMethod.new [String], nil, proc {|string| puts string}

string = QString.new "hello world"
show.call [string]
puts "\n\n"


# test case 2: QArray<String> auto-casts to String, iterating itself in the process

array = QArray.new String
array.push "El Bargo"
array.push "2 3 4"
show.call [array]
puts "\n\n"
