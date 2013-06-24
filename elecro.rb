
class Q
  @@types = []
  
  def self.addType type
    @@types << type
  end

  def self.addAs type, function
  end
end

class AddMethodTestClass
  add_method {:string => QString}
end
