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

  def test_quit
    assert @lib.quit == 'The library is now closed for renovations.', 'Quit message was wrong'
  end

end


class TC_Calendar < Test::Unit::TestCase
  def setup
    @cal = Calendar.instance
  end

  def teardown
    @cal._dump
  end

  def test_initialize_single_calendar
    assert_raise(NoMethodError) { Calendar.new }
  end

  def test_create_and_advance
    assert @cal.get_date == 0, 'date at creation should be 0'
    assert @cal.advance == 1, 'date after one advance() should be 1'
  end
end


class TC_Book < Test::Unit::TestCase
  def setup
    @b = Book.new(1, 'title', 'author')
  end
  def teardown
    @b = nil
  end

  def test_initialize_book_and_getters
    assert @b.get_id == 1, 'Book id should be 1'
    assert @b.get_title == 'title', 'Book title should be "title"'
    assert @b.get_author == 'author', 'Book author should be "author"'
  end

  def test_new_book_bad_id
    assert_raise(Exception) { Book.new('x', 't', 'a') }
  end

  def test_new_book_duck_id
    assert_nothing_raised { Book.new('3', 't', 'a') }
  end

  def test_get_duedate_for_new_book
    assert @b.get_due_date == nil, 'Due date should be nil for a new book'
  end

  def test_checkout_returns_nil
    assert @b.check_out(3) == nil, 'Checkout should return nil'
  end

  def test_checkout_bad_duedate
    assert_raise(Exception) { @b.check_out 'ten' }
  end

  def test_checkout_and_get_duedate
    @b.check_out 20
    assert @b.get_due_date == 20, 'Book due date should be 20'
  end

  def test_checkin
    @b.check_out(3)
    @b.check_in
    assert @b.get_due_date == nil, 'Due date of checked in book should be nil'
  end

  def test_to_s
    assert @b.to_s == '1: title, by author', 'Book.to_s returns wrong string'
  end
end


class TC_Member < Test::Unit::TestCase
  def setup
    @m = Member.new 'Bob', :lib
  end

  def teardown
    @m = nil
  end

  def test_initialize_and_get_name
    assert @m.get_name == 'Bob', 'New member should be called Bob'
  end

  def test_check_out_one_book
    @m.check_out 'a book'
    assert @m.get_books.size == 1, 'Member should only have one book checked out'
  end

  def test_get_books_empty
    assert @m.get_books == [], 'New member should not have any books'
  end
end