require 'singleton'
class Library
  # TODO All methods should return a result - string or int since they'll be used by the librarian
  # TODO All methods should take string or int and not objects
  include Singleton
  # TODO Creates data structure of books
  attr_reader :calendar

  def initialize
    @calendar = Calendar.instance
    @members = {}
    @current_member = nil
    @open = false
  end

  def open
    check_open_library
    @open = true
    @calendar.advance
    "Today is day #{@calendar.get_date}."
  end

  # TODO Formatted string of people with overdue books, including the overdue books
  # TODO If no overdue return appropriate string
  def find_all_overdue_books

  end

  # TODO If member in library, return string saying so, else add them to Hash and return string saying so
  # TODO Exception if library is closed
  def issue_card(name_of_member)
    memObj = Member.new name_of_member, self
    @members[name_of_member] = memObj
    "Library card issued to #{name_of_member}"
  end

  # TODO Set internal var 'current customer' to this member
  # TODO Ensure they are in the member Hash beforehand
  # TODO Return string signalling success or failure
  # TODO Exception if library is closed
  def serve(name_of_member)

  end

  # TODO Multiline string of current customer's overdue books (using book's to_s),  else return 'None'
  # TODO Exception if library is closed or nobody currently being served
  def find_overdue_books

  end

  # TODO Must be given at least one book to check in
  # TODO Take the book and 'return it to the collection', noting that this affects the Book, Member and Library objects
  # TODO Each book_number must be passed to search(str)
  # TODO Finally, return string saying member has return n books
  # TODO Exception if library is closed, no member being served, or member doesn't have that book id
  def check_in(*book_numbers)

  end

  # TODO Find books that are NOT checked out where string is in author/title
  # TODO string must be at least 4 chars long
  # TODO If multiple versions of a book exist, return only one
  # TODO Return multiline string of all the books via their to_s, or 'no books found', or 'search string must contain at least four characters'
  def search(string)

  end

  # TODO Param must have at least one book
  # TODO Checks out the book to the current member, or says why this isn't possible
  # TODO book_ids could have been returned by a call to search(str)
  # TODO Checking out affects the book, the member, and the library
  # TODO If successful, return 'n books have been checked out to member'
  # TODO Exception if library is closed, no current member, library doesn't have that book id
  # TODO Due dates must be current date + 7
  def check_out(*book_ids)

  end

  # TODO Param must have at least one book
  # TODO Sets member's books back to having due date +7 from today, or says why this isn't possible
  # TODO If successful 'n books have been renewed for member'
  # TODO Exception if library is closed, no current member, member doesn't have the book id
  def renew(*book_ids)

  end

  # TODO No other operations (except quit) should work when library is closed
  def close
    check_closed_library
    @open = false
    'Good night.'
  end

  def quit
    @open = false
    'The library is now closed for renovations.'
  end

  def check_closed_library
    raise Exception, 'The library is not open.', caller unless @open
  end
  def check_open_library
    raise Exception, 'The library is already open!', caller if @open
  end

  def self.reset
    @singleton__instance__ = nil
  end

  private :check_closed_library, :check_open_library
end


class Calendar
  include Singleton

  def initialize
    @date = 0
  end

  def get_date
    @date
  end

  def advance
    @date = @date.next
  end

  def self.reset
    @singleton__instance__ = nil
  end
end


class Book
  def initialize(id, title, author)
    int_check id
    @id = id
    @title = title
    @author = author
    @due_date = nil
  end

  def get_id
    @id
  end

  def get_title
    @title
  end

  def get_author
    @author
  end

  def get_due_date
    @due_date
  end

  def check_out(due_date)
    int_check due_date
    @due_date = due_date
    nil
  end

  def check_in
    @due_date = nil
  end

  def to_s
    "#{@id}: #{@title}, by #{@author}"
  end

  def int_check(num)
    raise Exception, 'Book ID must be an integer greater than 0!' if num.to_i == 0
  end
  private :int_check
end


class Member
  def initialize(name, library)
    @name = name
    @library = library
    @book_set = []
    @@MAX_BOOKS = 3
  end

  def get_name
    @name
  end

  def check_out(book)
    if @book_set.size < 3
      @book_set<<book
    else raise Exception, "Members cannot have more than #{@@MAX_BOOKS} books checked out. Cannot check in #{book.to_s}"
    end
  end

  def give_back(book)
    if @book_set.include? book
      @book_set.delete book
      "Returned #{book.to_s}"
    else
      "This member did not recently check out #{book.to_s}"
end
  end

  def get_books
    @book_set
  end

  def send_overdue_notice(notice)
    puts "#{@name}: #{notice}"
  end
end