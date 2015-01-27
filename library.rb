require 'singleton'
class Library
  include Singleton
  # TODO Creates data structure of books
  # TODO Creates a singleton calendar
  def initialize
    @members = {}
    @current_member = nil
    @open = false
  end

  # TODO Advance calendar and return welcome string
  def open
    check_open_library
    @open = true
  end

  # TODO Formatted string of people with overdue books, including the overdue books
  # TODO If no overdue return appropriate string
  def find_all_overdue_books()

  end

  # TODO If member in library, return string saying so, else add them to Hash and return string saying so
  # TODO Exception if library is closed
  def issue_card(name_of_member)

  end

  # TODO Set internal var 'current customer' to this member
  # TODO Ensure they are in the member Hash beforehand
  # TODO Return string signalling success or failure
  # TODO Exception if library is closed
  def serve(name_of_member)

  end

  # TODO Multiline string of current customer's overdue books (using book's to_s),  else return 'None'
  # TODO Exception if library is closed or nobody currently being served
  def find_overdue_books()

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

  # Return 'The library is now closed for renovations'
  def quit()
    @open = false
    'The library is now closed for renovations.'
  end

  def check_closed_library
    raise Exception, "The library is not open.", caller unless @open
  end
  def check_open_library
    raise Exception, "The library is already open!", caller if @open
  end

  private :check_closed_library, :check_open_library
end


class Calendar
  include Singleton

  def initialize
    @date = 0
  end

  def get_date()
    @date
  end

  def advance()
    @date = @date.next
  end
end


class Book
  # Should have an author, title, id and due_date
  # due_date is initially nil
  def initialize(id, title, author)

  end

  # Return book_id
  def get_id()

  end

  # Return title
  def get_title()

  end

  # Return author
  def get_author()

  end

  # Return due_date. Note that this could be nil
  def get_due_date()

  end

  # Set due_date. Return nothing
  # Should always work because books can be renewed (except going back in time)
  def check_out(due_date)

  end

  # Set due date to nil, return nothing
  # Book may already be checked in!
  def check_in

  end

  # id: title, by author
  def to_s

  end
end


class Member
  # New member should have no books, must have a lib card
  # Can have max of 3 books checked out at once
  # Should keep a reference to their library
  def initialize(name, library)

  end

  # Return name
  def get_name

  end

  # Add book to this member's checked out books
  def check_out(book)

  end

  # Remove book from member's checkout list
  # Possibly override 'return' keyword to be alias for this method?
  def give_back(book)

  end

  # Return set of books checkout out by this member
  # May be an empty set
  def get_books

  end

  # Tells member they have overdue books
  # Does this by printing string of name along with notice
  def send_overdue_notice(notice)

  end
end