require 'rake'

Gem::Specification.new do |s|
  s.name        = 'rpeg'
  s.version     = '0.1.0'
  s.summary     = "A Ruby port of LPEG, Lua's Parsing Expression Grammar library."
  s.description = <<~DESC
    As a learning excersice I have ported LPEG to Ruby. See the Readme at the github repo for details.
  DESC
  s.authors     = ['Rory Molinari']
  s.email       = 'rorymolinari@gmail.com'
  s.homepage    = 'https://github.com/rmolinari/rpeg'
  s.files       = FileList['lib/**/*.rb', 'CHANGELOG.md', 'README.md', 'Rakefile']
  s.license     = 'MIT'
  s.required_ruby_version = '~> 3.1.3'

  s.add_runtime_dependency 'must_be', '~> 1.1.0'

  s.add_development_dependency 'byebug', '~> 11.1.3'
end
