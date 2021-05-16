-- This allows you to type `:. path-to-file-without-.hs-extension`
-- to run the music in a file.
-- For further brevity, it's useful to define symlinks from where you run GHCI
-- to where your music is stored. This repo comes with one such symlink,
-- called "m". Thus I can run the sketch at "mtv-lang/sketches/1/1"
-- by calling ":. m/1/1".
:def! . readHsAsGhci

-- The following line demonstrates how to use the preceding.
--
-- PITFALL: This looks awkward; after all, why not instead include here
-- the code that is currently located in init-dispatch/go.hs?
-- But in fact doing that is hard, for reasons I don't understand/remember.
:. init-dispatch/go
