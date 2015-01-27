require_relative 'library'
require 'test/unit'

class TC_Library < Test::Unit::TestCase

  # Create library singleton for the tests
  def setup
    @lib = Library.instance
  end

  # Tear down the test library
  def teardown
    @lib = nil
  end

  def test_initialize_single_instance
    assert_raise(NoMethodError) do
      lib2 = Library.new
    end
  end

  # TODO
  def test_load_books
  end

  def test_close_on_closed
    assert_raise(Exception) do
      @lib.close
    end
  end

  def test_open_on_closed
    assert_nothing_raised do
      @lib.open
    end
  end

end