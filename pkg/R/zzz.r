#.First.lib <- function(lib, pkg)
.onLoad <- function(lib, pkg)
{
    library.dynam("utf8latex", pkg, lib)
}
