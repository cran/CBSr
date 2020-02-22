.onLoad = function(libname, pkgname) {
  rJava::.jpackage(pkgname, lib.loc = libname)
  rJava::.jaddClassPath(system.file("java",package="CBSr"))
}
