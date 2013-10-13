# codswallop.rb I'm trying to understand what subject-oriented programming is.  I think this is it??

class Entity
  def properties
    @properties ||= {}
  end

  def set key, value
    properties[key] = value
  end

  def get key
    properties[key]
  end
end
