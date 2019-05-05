# Build against libtiff compiled with Rtools
if (!file.exists("../windows/libtiff-4.0.9/include/tiff.h")) {
  if(getRversion() < "3.3.0") setInternet2()
  download.file("https://github.com/rwinlib/libtiff/archive/v4.0.9.zip",
                "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
