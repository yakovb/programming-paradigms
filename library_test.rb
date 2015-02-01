require_relative 'library'
require 'test/unit'
# TODO make assert_raise methods inspect their error messages as well (if they have messages)
class TC_Library < Test::Unit::TestCase

  def setup
    @lib = Library.instance
  end

  def teardown
    Library.reset
    Calendar.reset
  end

  def test_initialize_single_library
    assert_raise(NoMethodError) { Library.new }
  end

  def test_initialize_makes_calendar
    assert_nothing_raised { @lib.calendar.get_date }
  end

  def test_load_all_books
    assert @lib.books.size == 201, 'Did not load all 200 books in collection'
  end

  def test_load_books_as_objects
    str = @lib.books[198].to_s
    assert str == '199: My Test Book, by Yakov Boglev', "Returned: #{str}"
  end

  #TODO no overdue books
  #TODO closed library

  def test_all_overdue_one_member
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    10.times {@lib.calendar.advance}
    res = "member bob overdue list:\n--------------------\n#{@lib.books[0].to_s}\n#{@lib.books[99].to_s}\n"
    str = @lib.find_all_overdue_books
    assert str == res, "Returned: #{str}"
  end

  def test_all_overdue_two_members
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    @lib.issue_card('alice')
    @lib.serve('alice')
    @lib.check_out(2, 200)
    10.times {@lib.calendar.advance}
    res = "member bob overdue list:\n--------------------\n#{@lib.books[0].to_s}\n#{@lib.books[99].to_s}" +
          "\n\nmember alice overdue list:\n--------------------\n#{@lib.books[1].to_s}\n#{@lib.books[199].to_s}\n"
    str = @lib.find_all_overdue_books
    assert str == res, "Returned: #{str}"
  end

  def test_all_overdue_no_members
    @lib.open
    str = @lib.find_all_overdue_books
    assert str == 'No books are overdue.', "Returned: #{str}"
  end

  def test_all_overdue_library_closed
    ex = assert_raise(Exception) {
      @lib.find_all_overdue_books
    }
    assert ex.message == 'The library is not open.', "Returned: #{ex.message}"
  end

  def test_search_unique_book
    @lib.open
    res = @lib.search("violent bear")
    assert res == @lib.books[184].to_s, "Returned: #{res}"
  end
  #TODO search unique book
  #TODO search title of multi copy book, one available
  #TODO search title of multi copy book, all available
  #TODO search title of multi copy book, none available
  #TODO search title of multi copy book, multitple authors, all available
  #TODO search title of multi copy book, multitple authors, some available
  #TODO search title of multi copy book, multitple authors, none available
  #TODO search no result
  #TODO search < 4 chars

  def test_find_overdue_books_where_exist
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    10.times {@lib.calendar.advance}
    str = @lib.find_overdue_books
    assert str == @lib.books[0].to_s + "\n" + @lib.books[99].to_s, "Returned #{str}"
  end

  def test_find_overdue_books_where_none
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    str = @lib.find_overdue_books
    assert str == 'None', "Returned #{str}"
  end

  def test_find_overdue_books_no_member
    assert_raise(Exception) {
      @lib.open
      @lib.find_overdue_books
    }
  end

  def test_find_overdue_books_library_closed
    assert_raise(Exception) { @lib.find_overdue_books }
  end

  def test_checkin_books
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(200, 1)
    str = @lib.check_in(200, 1)
    assert str == 'bob has returned 2 books.', "Returned: #{str}"
    assert @lib.books[199].get_due_date == nil, 'Book 200 not checked in'
    assert @lib.books[0].get_due_date == nil, 'Book 1 not checked in'
  end

  def test_checkin_books_not_out
    ex = assert_raise(Exception) {
      @lib.open
      @lib.issue_card('bob')
      @lib.serve('bob')
      @lib.check_in(200, 1)
    }
    assert ex.message == 'The member does not have book 200.', "Returned: #{ex.message}"
  end

  def test_checkin_no_books
    ex = assert_raise(Exception) {
      @lib.open
      @lib.issue_card('bob')
      @lib.serve('bob')
      @lib.check_in
    }
    assert ex.message == 'Cannot check in zero books', "Returned: #{ex.message}"
  end

  def test_checkin_no_member
    ex = assert_raise(Exception) {
      @lib.open
      @lib.check_in(1)
    }
    assert ex.message == 'No member is currently being served.', "Returned: #{ex.message}"
  end

  def test_checkin_library_closed
    ex = assert_raise(Exception) {
      @lib.check_in(1)
    }
    assert ex.message == 'The library is not open.', "Returned: #{ex.message}"
  end

  def test_checkin_search_result
    res = '200: My Test Book, by Yakov Boglev'
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(200)
    str = @lib.check_in(res)
    assert str == 'bob has returned 1 books.', "Returned: #{str}"
  end

  def test_check_out_one_book
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    str = @lib.check_out(200)
    assert str == '1 books have been checked out to bob.', "Returned: #{str}"
  end

  def test_check_out_three_books
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    str = @lib.check_out(1, 100, 201)
    assert str == '3 books have been checked out to bob.', "Returned: #{str}"
  end

  def test_check_out_too_many_books
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    str = @lib.check_out(1, 100, 150, 201)
    assert str == 'Members cannot check out more than 3 books', "Returned: #{str}"
  end

  def test_check_out_no_books
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    str = @lib.check_out
    assert str == 'You cannot check out zero books', "Returned: #{str}"
  end

  def test_check_out_affects_member
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    assert @lib.members['bob'].get_books.size == 2, 'Bob should have 2 books checked out'
  end

  def test_check_out_affects_books
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    assert @lib.books[0].get_due_date == @lib.calendar.get_date + 7, 'Book 1 was not checked out properly'
    assert @lib.books[99].get_due_date == @lib.calendar.get_date + 7, 'Book 100 was not checked out properly'
    assert @lib.books[200].get_due_date == nil, 'Book 201 should not have been checked out'
  end

  def test_check_out_library_closed
    assert_raise(Exception) { @lib.check_out(1) }
  end

  def test_check_out_no_current_member
    assert_raise(Exception) {
      @lib.open
      @lib.check_out(1)
    }
  end

  def test_check_out_bad_id
    assert_raise(Exception) {
      @lib.open
      @lib.issue_card('bob')
      @lib.serve('bob')
      @lib.check_out(500)
    }
  end

  def test_check_out_passing_in_search_result
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    result = @lib.books[1].to_s + "\n" + @lib.books[200].to_s
    str = @lib.check_out(result)
    assert str == '2 books have been checked out to bob.', "Returned: #{str}"
  end

  def test_renew_books
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 100)
    str = @lib.renew(100, 1)
    assert str == '2 books have been renewed for bob.', "Returned: #{str}"
  end

  def test_renew_books_not_checked_out
    ex = assert_raise(Exception) {
      @lib.open
      @lib.issue_card('bob')
      @lib.serve('bob')
      @lib.renew(100, 1)
    }
    assert ex.message == 'The member does not have book 100.', "Returned: #{ex.message}"
  end

  def test_renew_books_some_checked_out
    ex = assert_raise(Exception) {
      @lib.open
      @lib.issue_card('bob')
      @lib.serve('bob')
      @lib.check_out(100)
      @lib.renew(100, 1)
    }
    assert ex.message == 'The member does not have book 1.', "Returned: #{ex.message}"
  end

  def test_renew_books_on_search
    @lib.open
    @lib.issue_card('bob')
    @lib.serve('bob')
    @lib.check_out(1, 200)
    result = @lib.books[0].to_s + "\n" + @lib.books[199].to_s
    str = @lib.renew(result)
    assert str == '2 books have been renewed for bob.', "Returned: #{str}"
  end

  def test_renew_zero_books
    ex = assert_raise(Exception) {
      @lib.open
      @lib.issue_card('bob')
      @lib.serve('bob')
      @lib.renew
    }
    assert ex.message == 'Cannot renew zero books', "Returned: #{ex.message}"
  end

  def test_renew_library_closed
    ex = assert_raise(Exception) {
      @lib.renew
    }
    assert ex.message == 'The library is not open.', "Returned: #{ex.message}"
  end

  def test_renew_no_member
    ex = assert_raise(Exception) {
      @lib.open
      @lib.renew
    }
    assert ex.message == 'No member is currently being served.', "Returned: #{ex.message}"
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

  def test_open_on_closed_with_msg
    str = @lib.open
    assert str == 'Today is day 1.', "Message should be 'Today is day 1.' but was '#{str}'"
  end

  def test_open_on_open
    assert_raise(Exception) { 2.times {@lib.open} }
  end

  def test_issue_card_to_new_member
    @lib.open
    str = @lib.issue_card('person')
    assert str == 'Library card issued to person', "Returned wrong string: #{str}"
  end

  def test_issue_card_to_existing_member
    @lib.open
    @lib.issue_card('person')
    str = @lib.issue_card('person')
    assert str == 'person already has a library card.', "Returned wrong string: #{str}"
  end

  def test_issue_card_library_closed
    assert_raise(Exception) { @lib.issue_card('person') }
  end

  def test_serve_existing_member
    @lib.open
    @lib.issue_card('person')
    assert @lib.serve('person') == 'Now serving person.'
  end

  def test_serve_non_existent_member
    @lib.open
    str = @lib.serve('person')
    assert str == 'person does not have a library card.', "Returned wrong string: #{str}"
  end

  def test_serve_on_library_closed
    assert_raise(Exception) { @lib.serve('person') }
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
    Calendar.reset
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

  class DummyBook
    attr_accessor :id, :title, :author
    def initialize(id, title, author)
      @id = id
      @title = title
      @author = author
    end
    def to_s
      "Dummy Book"
    end
  end

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

  def test_check_out_4_books
    assert_raise(Exception) { 4.times { @m.check_out 'a book' } }
  end

  def test_give_back_success_message
    b = DummyBook.new(1, 'aTitle', 'aAuthor')
    @m.check_out(b)
    assert @m.give_back(b) == 'Returned Dummy Book', 'Giving back a book should return string signifying success'
  end

  def test_give_back_successful_removal
    b = DummyBook.new(1, 'aTitle', 'aAuthor')
    @m.check_out(b)
    @m.give_back(b)
    assert @m.get_books == [], 'Books array should be empty after 1 book is checked out then checked back in'
  end

  def test_give_back_unsuccessful_message
    b = DummyBook.new(1, 'aTitle', 'aAuthor')
    assert @m.give_back(b) == 'This member did not recently check out Dummy Book', 'Giving back a book the member does not have should return failure message'
  end

  def test_get_books_empty
    assert @m.get_books == [], 'New member should not have any books'
  end
end