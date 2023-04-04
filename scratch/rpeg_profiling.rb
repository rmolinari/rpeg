require_relative '../lib/rpeg'
require 'byebug'

# Do some profiling on the RPeg code using a search of the King James bible as discussed in Section 5.2 of Ierusamilschy, _A Text
# Pattern-Matching Tool based on Parsing Expression Grammars_, 2008.

class Searcher FILE_NAME = 'King_James.txt'

  def initialize
    @text = File.read('King_James.txt')
  end

  # Search for the given pattern in the text of the King James
  def search(patt)
    # First compile the pattern and embed it in a grammar to search for it anywhere in the string
    p = RPEG::RE.compile("'#{patt}'")
    g = RPEG.P([p + 1 * RPEG.V(0)])

    g.match(@text)
  end
end

searcher = Searcher.new

if ENV['stackprof']
  require 'stackprof'

  StackProf.run(mode: :cpu, raw: true, out: 'profile/stackprof-cpu-rpeg.dump') do
    puts searcher.search('Alpha ')
  end
elsif ENV['rubyprof']
  require 'ruby-prof'
  tag = 'rpeg_search'

  profile = RubyProf::Profile.new

  profile.exclude_common_methods!
  profile.exclude_methods!([/Array#/, /Rational#/, /Integer#/, /Enumerator#/, /Range#/, /Fixnum#/, /Enumerable#/])

  profile.start
  puts searcher.search('Alpha ')
  profile.stop

  FileUtils.mkdir_p("profile")
  flat_printer = RubyProf::FlatPrinter.new(profile)
  graph_printer = RubyProf::GraphPrinter.new(profile)
  call_tree_printer = RubyProf::CallTreePrinter.new(profile)
  stack_printer = RubyProf::CallStackPrinter.new(profile)

  File.open("profile/flat_#{tag}.out",  "w") {|f| flat_printer.print(f)}
  File.open("profile/graph_#{tag}.out", "w") {|f| graph_printer.print(f)}

  # Just to annoy me, the CallTreePrinter class now does paths differently and in a way
  # that is poorly documented.
  call_tree_printer.print(path: "profile", profile: "#{tag}")
  File.open("profile/stack_#{tag}.html", 'w') {|f| stack_printer.print(f)}
else
  # Just run it for elapsed time
  puts searcher.search('Alpha ')
end
