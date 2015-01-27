require_relative 'library'
require 'test/unit'

class TC_Library < Test::Unit::TestCase

  # Create library singleton for the tests
  def setup
    lib = Library.instance
  end

  # Tear down the test library
  def teardown
    lib = nil
  end

  def test_initialize_single_instance
    assert_raise(NoMethodError) do
      lib2 = Library.new
    end
  end

end