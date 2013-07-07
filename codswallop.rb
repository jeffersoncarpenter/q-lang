# codswallop.rb I'm trying to understand what subject-oriented programming is.  I think this is it??

class Ref
  attr_accessor :val

  def initialize val
    @val = val
  end

  def to_s
    @val.to_s
  end
end

class Subject
  # Subject provides set and get methods

  def self.properties
    @@properties ||= {}
  end

  def hash
    @@properties[self.class]
  end

  def set key, value
    @hash[key] = value
  end

  def get key
    @hash[key]
  end
end

class Entity
end
