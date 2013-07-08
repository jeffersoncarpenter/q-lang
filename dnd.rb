# dnd.rb is a thing to help me with my dnd campaign
# it is a test case for electro.rb

require_relative 'electro.rb'
require_relative 'codswallop.rb'



# this subject allows entities to have a color

class Colored < Subject

  define :color, proc {Ref.new String}


  def self.included type
    Subject.import_into type
    type.implement Serializable, proc { |colored|
      colored.color
    }
  end

  def color
    ref = get :color
  end
end


# make a new blank entity
c = Entity.new

# cast it to a Colored object and set its color
test = c.as Colored
test.color.val = "blue"


show = QMethod.new [String], nil, proc {|string| puts string}

puts test.color.class
show.call [test.color]


