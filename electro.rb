class Test
  def self.test
    yield
    puts "\n\n"
  end
end

module QType
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
    
    begin
      ordered_args = match constraints, args

      invoke ordered_args, @param_types.dup do |args_as_types|
        @proc.call args_as_types
      end
    rescue
      arg_types = args.map {|arg| arg.class}
      puts "Couldn't execute method taking parameter types #{@param_types} with arguments of types #{arg_types}"
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


class Functor
  include QType
  
  def initialize type
    implement type, proc { |functor, continuation|
      functor.fmap continuation
    }
  end
end


class QArray < Functor
  attr_reader :array

  def initialize type
    @type = type
    @array = []
    super
  end

  def fmap func
    qArray = QArray.new @type
    @array.each do |object|
      qArray.push func.call object
    end
    qArray
  end

  def push object
    if object.class == @type
      @array << object
    end
  end
end


class Task < Functor
  def initialize type
    @type = type
    @funcs = []
    super
  end

  def fmap continuation
    task = Task.new @type
    done do |result|
      task.resolve continuation.call result
    end
    task
  end

  def done &block
    if @result
      yield value
    else
      @funcs << proc {|result| yield result}
    end
  end

  def resolve result
    @result = result
    @funcs.each do |func|
      func.call result
    end
  end
end


# to run the test cases, you look at the output and see if it's right

# function used in early test cases

show = QMethod.new [String], nil, proc {|string| puts string}


# test case 0: function works

Test.test do
  show.call ["aoeu"]
end

# test case 1: QString type auto-casts to String

Test.test do
  string = QString.new "hello world"
  show.call [string]
end

# test case 2: QArray<String> auto-casts to String, iterating itself in the process

Test.test do
  array = QArray.new String
  array.push "El Bargo"
  array.push "2 3 4"
  show.call [array]
end

# test case 3: QArray<QString> auto-casts to String

Test.test do
  array = QArray.new QString
  array.push (QString.new "El Mango")
  array.push (QString.new "Hobo")
  show.call [array]
end

# test case 4: Task<T> can be resolved

Test.test do
  task = Task.new String
  task.done do |string|
    puts string
  end
  task.resolve "IT IS NOT THE CASE THAT"
  puts "TASKS DONT WORK"
end

# test case 5: Task<T> auto-casts to T

Test.test do
  task = Task.new String
  show.call [task]
  task.resolve "1 == 2 AND"
  puts "TASKS DONT AUTO_CAST"
end
