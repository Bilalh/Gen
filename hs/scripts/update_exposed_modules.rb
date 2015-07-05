#!/usr/bin/env ruby
# Update the exposed-modules in gen-essence.cabal

mods = Dir["src/Gen/**/*.hs"].each do |g|
    g.sub!("src/", '').sub!(/.hs$/, '').gsub!('/', '.')
end.delete_if { |e| e =~ /GHCI/ }

cabal=File.read "essence-gen.cabal"
cabal[/ +--MODULES.*--MODULES END\n/m]= <<-SHELL
    --MODULES
    exposed-modules  : Build_autoversion
                     , Paths_gen_essence
                     , #{mods.join("\n                     , ") }
    --MODULES END
    SHELL

File.write "essence-gen.cabal", cabal