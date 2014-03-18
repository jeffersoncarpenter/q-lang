class Super
  def self.add_can_be type_function, impl
  end

  # returns a list of all types that object could be
  def self.can_be object
  end
  
  def self.find_impl_for type, object
    Proc.new do |func|
    end
  end
end


class Functor
  # type :: Class
  # get_internal_type :: type -> Class
  # fmap :: func -> type -> type
  def self.add type, get_internal_type, fmap
    @@get_internal_type ||= {}
    @@fmap ||= {}

    @@get_internal_type[type] = get_internal_type
    @@fmap[type] = fmap
  end

  def self.get_internal_type type, functor
    @@get_internal_type[type] functor
  end

  def self.get_fmap type, functor
    @@fmap[type]
  end
end


functor_can_be = Proc.new do |obj|
  Functor.get_internal_type obj
end


isFunctor = Proc.new {|type| Functor.is type}
asFunctor = Proc.new do |func, object|
  fmap = Functor.fmap_impl object.class
  fmap func, object
end


Super.can_be_func isFunctor, asFunctor


def super func, types
  Proc.new do |*args|
    
  end
end


puts "aoeu"
