class Library
  # Should be a single object
  # Creates data structure of books
  # Creates a singleton calendar
  # Creates empty Hash of members
  # Set flag to 'not open'
  # Set current member being served to nil
  def initialize()

  end

  # If already open throw exception
  # else advance calendar and return welcome string
  def open()

  end

  # Formatted string of people with overdue books,
  # including the overdue books
  # If no overdue return appropriate string
  def find_all_overdue_books()

  end

  # If member in library, return string saying so
  # else add them to Hash and return string saying so
  # Exception if library is closed
  def issue_card(name_of_member)

  end

  # Set internal var 'current customer' to this member
  # Ensure they are in the member Hash beforehand
  # Return string signalling success or failiure
  # Exception if library is closed
  def serve(name_of_member)

  end

  # Multiline string of current customer's overdue books (using book's to_s)
  # else return 'None'
  # Exception if library is closed or nobody currently being served
  def find_overdue_books()

  end

  # Must be given at least one book to check in
  # Take the book and 'return it to the collection',
  # noting that this affects the Book, Member and Library objects
  # Each book_number must be passed to search(str)
  # Finally, return string saying member has return n books
  # Exception if library is closed, no member being served, or member doesn't have that book id
  def check_in(*book_numbers)

  end

  # Find books that are NOT checked out where string is in author/title
  # string must be at least 4 chars long
  # If multiple versions of a book exist, return only one
  # Return multiline string of all the books via their to_s, or 'no books found'
  # or 'search string must contain at least four characters'
  def search(string)

  end

  # Param must have at least one book
  # Checks out the book to the current member, or says why this isn't possible
  # book_ids could have been returned by a call to search(str)
  # Checking out affects the book, the member, and the library
  # If successful, return 'n books have been checked out to member'
  # Exception if library is closed, no current member, library doesn't have that book id
  def check_out(*book_ids)

  end

  # Param must have at least one book
  # Sets member's books back to having due date +7 from today
  # or says why this isn't possible
  # If successful 'n books have been renewed for member'
  # Exception if library is closed, no current member, member doesn't have the book id
  def renew(*book_ids)

  end

  # Success means return 'Good night'
  # Exception if library is closed
  # No other operations (except quit) should work when library is closed
  def close()

  end

  # Return 'The library is now closed for renovations'
  def quit()

  end
end


class Calendar
  # Should be singleton object
  # Time measured in ints starting at 0
  def initialize()

  end

  # Return current date
  def get_date()

  end

  # Increment date and return new date
  def advance()

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