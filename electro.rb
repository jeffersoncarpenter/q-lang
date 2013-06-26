class Test
  def self.test
    yield
  end
end

module QType
  # it goes implementations[implementer][implemented]

  # you can programmatically have your objects implement things using these methods added by the mixin

  def implementations
    @implementations ||= {}
  end

  # implementation is a proc that takes your object and returns the new type
  # this means you can only use public stuff to implement stuff
  def implement type, implementation
    implementations[type] = implementation
  end
  
  def implements? type
    implementations[type] || (self.class.implements? type)
  end
    

  # a type can implement things using these class methods

  def QType.Implementations
    @implementations ||= {}
  end

  def self.included(base)
    base.extend(ClassMethods)
  end

  module ClassMethods
    def implementations
      @implementations ||= {}
    end

    def implements? type
      return true if self == type
      implementations.keys.index {|type| type.implements? type}
    end
    
    def implement type, implementation
      implementations[type] = implementation
    end
  end


  # the as function is what you use to cast to a type

  # note that you invert control by passing it a continuation
  # the point of Q is to generate these continuations automatically

  def as type, &block
    return yield self if self.class == type
    
    if implementation = implementations[type]
      return implementation.call self, proc {|cast_type| yield cast_type}
    end

    implementation = self.class.implementations[type]
    implementation.call self, proc {|cast_type| yield cast_type} if implementation
  end
end

class Object
  include QType
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
    puts
    puts objects
    raise "Unable to find match"
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
      proc {|object| object.implements? type}
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

class QString
  attr_reader :string

  include QType

  implement String, proc {|qString, continuation| continuation.call qString.string }
  
  def initialize string
    @string = string
  end
end

class QArray
  attr_reader :array

  include QType

  def initialize type
    @type = type
    @array = []

    implement type, proc {|qArray, continuation| qArray.array.each do |object|
        continuation.call object
      end}
  end

  def push object
    if object.class == @type
      @array << object
    end
  end
end



# function used in early test cases

show = QMethod.new [String], nil, proc {|string| puts string}


# test case 0: function works

Test.test do
  show.call ["aoeu"]
  puts "\n\n"
end

# test case 1: QString type auto-casts to String

Test.test do
  string = QString.new "hello world"
  show.call [string]
  puts "\n\n"
end

# test case 2: QArray<String> auto-casts to String, iterating itself in the process

Test.test do
  array = QArray.new String
  array.push "El Bargo"
  array.push "2 3 4"
  show.call [array]
  puts "\n\n"
end

# test case 3: QArray<QString> auto-casts to String

Test.test do
  array = QArray.new QString
  array.push (QString.new "El Mango")
  array.push (QString.new "Hobo")
  show.call [array]
  puts "\n\n"
end
