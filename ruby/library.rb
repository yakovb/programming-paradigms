require 'singleton'
class Library
  include Singleton
  attr_reader :calendar, :books, :members

  def initialize
    @books = load_books('collection.txt')
    @calendar = Calendar.instance
    @members = {}
    @current_member = nil
    @open = false
  end

  def open
    check_open_library
    @open = true
    "Today is day #{@calendar.advance}."
  end

  def find_all_overdue_books
    check_closed_library
    result = []
    @members.each_pair do |memberName, memberObject|
      result<<("member " + memberName + " overdue list:" +
          "\n--------------------\n" + (find_overdue_books memberObject) + "\n")
    end
    (result.empty?) ? 'No books are overdue.' : result.join("\n")
  end

  def issue_card(name_of_member)
    check_closed_library
    if @members.include? name_of_member
      "#{name_of_member} already has a library card."
    else
      memberObject = Member.new name_of_member, self
      @members[name_of_member] = memberObject
      "Library card issued to #{name_of_member}"
    end
  end

  def serve(name_of_member)
    check_closed_library
    if @members.include? name_of_member
      @current_member = @members[name_of_member]
      "Now serving #{name_of_member}."
    else
      "#{name_of_member} does not have a library card."
    end
  end

  def find_overdue_books(mem = @current_member)
    check_closed_library
    check_current_member
    result = mem.get_books.select {
        |b| b if b.get_due_date != nil && b.get_due_date < @calendar.get_date }
    (result.size > 0) ? result.join("\n") : 'None'
  end

  def check_in(*book_numbers)
    check_closed_library
    check_current_member
    raise Exception, 'Cannot check in zero books', caller if book_numbers.empty?

    id_array = book_numbers
    id_array = search_to_array book_numbers if book_numbers[0].class == String
    badId = -1
    if id_array.all? do |id|
      badId = id
      @current_member.get_books.include?(@books[id-1])
    end
      id_array.each do |id|
        b = @books[id-1]
        @current_member.give_back(b)
        b.check_in
      end
      "#{@current_member.get_name} has returned #{id_array.size} books."
    else
      raise Exception, "The member does not have book #{badId}.", caller
    end
  end

  def search(string)
    check_closed_library
    if string.size < 4
      'Search string must contain at least four characters.'
    else
      key = string.downcase
      res = @books.select do |b|
        b.get_due_date == nil &&
            (b.get_title.downcase.include?(key) || b.get_author.downcase.include?(key))
      end
      res.uniq! { |b| b.get_title + b.get_author }
      (res.empty?) ? 'No books found.' : res.join("\n")
    end
  end

  def check_out(*book_ids)
    check_closed_library
    check_current_member
    id_array = book_ids
    id_array = search_to_array book_ids if book_ids[0].class == String
    badId = -1
    if id_array.size > 3
      'Members cannot check out more than 3 books'
    elsif id_array.empty?
      'You cannot check out zero books'
    elsif id_array.any? do |id|
      badId = id
      id < 1 || id > @books.size
    end
      raise Exception, "The library does not have book #{badId}", caller
    else
      id_array.each do |id|
        b = @books[id - 1]
        b.check_out(@calendar.get_date + 7)
        @current_member.check_out(b)
      end
      "#{id_array.size} books have been checked out to #{@current_member.get_name}."
    end
  end

  def renew(*book_ids)
    check_closed_library
    check_current_member
    id_array = book_ids
    raise Exception, 'Cannot renew zero books', caller if id_array.empty?
    id_array = search_to_array book_ids if book_ids[0].class == String
    badId = -1

    if id_array.all? do |id|
      badId = id
      @current_member.get_books.include?(@books[id-1])
    end
      id_array.each do |id|
        b = @books[id-1]
        b.check_out(@calendar.get_date + 7)
      end
      "#{id_array.size} books have been renewed for #{@current_member.get_name}."
    else
      raise Exception, "The member does not have book #{badId}.", caller
    end
  end

  def close
    check_closed_library
    @open = false
    'Good night.'
  end

  def quit
    @open = false
    'The library is now closed for renovations.'
  end

  def self.reset
    @singleton__instance__ = nil
  end

  # Private Library methods start from here
  #########################################
  def check_closed_library
    raise Exception, 'The library is not open.', caller unless @open
  end
  def check_open_library
    raise Exception, 'The library is already open!', caller if @open
  end
  def load_books(src)
    rawArr = File.readlines src
    rawArr[-1] = rawArr.last+"\n" if rawArr.last[-1] != "\n"
    rawArr.map do |b|
      auth, *tail = b[1..-3].rpartition(',')
      Book.new(rawArr.index(b)+1, auth, tail.last)
    end
  end
  def check_current_member
    raise Exception, 'No member is currently being served.', caller if @current_member == nil
  end
  def search_to_array(search)
    search_arr = search[0].split("\n")
    search_arr.map { |res| res.partition(':').first.to_i }
  end

  private :check_closed_library, :check_open_library, :load_books, :check_current_member, :search_to_array
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
    raise Exception, "One or more numeric values are required for this operation, which you didn't provide" if num.to_i == 0
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