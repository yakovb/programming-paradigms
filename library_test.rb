require_relative 'library'
require 'test/unit'

class TC_Library < Test::Unit::TestCase

  # Create library singleton for the tests
  def setup
    @lib = Library.instance
  end

  # Tear down the test library
  def teardown
    @lib._dump(-1)
  end

  def test_initialize_single_instance
    assert_raise(NoMethodError) { lib2 = Library.new }
  end

  # TODO
  def test_load_books
  end

  def test_close_on_closed
    assert_raise(Exception) { @lib.close }
  end

  def test_close_on_open
    assert_nothing_raised {
      @lib.open
      @lib.close
    }
  end

  def test_open_on_closed
    assert_nothing_raised { @lib.open }
  end

  def test_open_on_open
    assert_raise(Exception) { 2.times @lib.open }
  end

end