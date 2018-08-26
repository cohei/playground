export PATH="$gnutar/bin:$clang/bin:$coreutils/bin:$gawk/bin:$gzip/bin:$gnugrep/bin:$gnused/bin:$binutils/bin:$gnumake/bin"
tar -xzf $src
cd hello-2.10
./configure --prefix=$out
make
make install
