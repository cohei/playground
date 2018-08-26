#! /usr/bin/env python

import sys, os, urllib, gzip

def GetAndGzip(url, out):
    '''GetAndGzip(url, out)
    urlを読み込み、gzipで圧縮してファイル out に出力する
    '''
    remote = urllib.urlopen(url).read()
    gzfile = gzip.GzipFile(filename='', mode='wb', fileobj=out)
    gzfile.write(remote)
    gzfile.close()

if len(sys.argv) <> 2:
    print '使い方: python wgetgz.py url'
else:
    url = sys.argv[1]
    GetAndGzip(url, sys.stdout)
