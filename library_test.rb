require_relative 'library'
require 'test/unit'

class TC_Library < Test::Unit::TestCase

  def setup
    @lib = Library.instance
  end

  def teardown
    @lib._dump(-1)
  end

  def test_initialize_single_library
    assert_raise(NoMethodError) { Library.new }
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


class TC_Calendar < Test::Unit::TestCase
  def setup
    @cal = Calendar.instance
  end

  def teardown
    @cal._dump(-1)
  end

  def test_initialize_single_calendar
    assert_raise(NoMethodError) { Calendar.new }
  end

  def test_date_at_creation
    assert @cal.get_date == 0
  end
end