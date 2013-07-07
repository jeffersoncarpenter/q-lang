# dnd.rb is a thing to help me with my dnd campaign
# it is a test case for electro.rb

require_relative 'electro.rb'
require_relative 'codswallop.rb'


class Colored < Subject
  def self.included type
    Subject.import_into type
    type.implement Serializable, proc { |colored|
      colored.color
    }
  end

  def color
    get color
  end

  def color= val
    set color, val
  end
end



c = Entity.new

test = c.as Colored

test.color = "blue"
