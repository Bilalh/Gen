#!/usr/bin/env ruby
# Update the exposed-modules in essence-gen.cabal

mods = Dir["src/Gen/**/*.hs"].each do |g|
    g.sub!("src/", '').sub!(/.hs$/, '').gsub!('/', '.')
end

cabal=File.read "essence-gen.cabal"
cabal[/ +--MODULES.*--MODULES END/m]= <<-SHELL
    --MODULES 
    exposed-modules  : Build_autoversion
                     , Paths_essence_gen
                     , #{mods.join("\n                     , ") }
    --MODULES END
    SHELL

File.write "essence-gen.cabal", cabal