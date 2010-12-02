#!/usr/bin/env clbuild lisp --script

(setq *print-case* :downcase)
(defvar *asdf-dir* "../asdf/")
(defvar *src-dir* "../src")

(defvar *py-modules*
  '(string
    re
    struct
    difflib
    textwrap
    codecs
    unicodedata
    stringgrep
    fpformat
    datetime
    calender
    collections
    heapq
    bisect
    array
    sched
    queue
    weakref
    types
    copy
    pprint
    repr
    math
    cmath
    decimal
    fractions
    random
    itertools
    functools
    os.path
    fileinput
    stat
    statvfs
    filecmp
    tempfile
    glob
    fnmatch
    linecache
    shutil
    pickle
    copy-reg
    shelve
    mershal
    anydbm
    whichdb
    dbm
    gdbm
    dumbdbm
    sqlite3
    zlib
    gzip
    bz2
    zipfile
    tarfile
    csv
    Config-Parser
    robotparser
    netrc
    xdrlib
    plistlib
    hashlib
    hmac
    os
    io
    time
    argparse
    getopt
    logging
    getpass
    curses
    curses.textpad
    curses.wrapper
    curses.ascii
    curses.panel
    platform
    errno
    ctypes
    select
    threding
    thread
    dummy-threading
    dummy-thread
    multiprocessing
    mmap
    readline
    rlcompleter
    subprocess
    socket
    ssl
    signal
    asyncore
    asynchat
    email
    json
    mailcap
    mailbox
    mimetypes
    base64
    binshex
    binascii
    quopri
    uu
    HTML-Parser
    htmlentitydefs
    xml.parsers.expat
    xml.dom
    xml.dom.minidom
    xml.dom.pulldom
    xml.sax
    xml.sax.handler
    xml.sax.saxutils
    xml.sax.xmlreader
    xml.etree.ElementTree
    webbrowser
    cgi
    cgitb
    wsgiref
    urllib
    urllib2
    httpilb
    ftplib
    poplib
    imaplib
    nntplib
    smtplib
    smtpd
    telnetlib
    uuid
    urlparse
    Socket-Server
    Base-HTTP-Server
    Simple-HTTP-Server
    CGI-HTTP-Server
    cookielib
    Cookie
    xmlrpclib
    Simple-XML-RPC-Server
    Doc-XML-RPC-Server
    audioop
    aifc
    sunau
    wave
    chunk
    colorsys
    imghdr
    sndhdr
    ossaudiodev
    gettext
    locale
    cmd
    shlex
    Tkinter
    tkk
    tix
    Scrolled-Text
    turtle
    doctest
    unittest
    test
    test.test-support
    sys
    sysconfig
    builtin
    main
    warnings
    contextlib
    abc
    atexit
    traceback
    gc
    inspect
    site
    fpectl
    distutils
    formatter
    mslib
    msvcrt
    winreg
    winsound
    posix
    pwd
    spwd
    grp
    crypt
    termios
    tty
    pty
    fcntl
    pipes
    resource
    nis
    syslog
    ic
    Mac-OS
    macostools
    findertools
    Easy-Dialogs
    Frame-Work
    auto-GIL
    Color-Picker
    ))

(defun clap-module-sym (sym)
  (let ((name (string sym)))
    (intern (format nil "CLAP-~A" name))))

;; should use pathname
(defun clap-top-asdf-file ()
  (make-pathname :name "clap"
                 :type "asd"
                 :directory `(:relative ,*asdf-dir*)))

(defun clap-asdf-file (sym)
  (make-pathname :name (string-downcase (string sym))
                 :type "asd"
                 :directory `(:relative ,*asdf-dir*)))

(defun clap-package-file (orgsym sym)
  (make-pathname :name (string-downcase (string sym))
                 :type "lisp"
                 :directory `(:relative ,*src-dir*
                                        ,(string-downcase (string orgsym)))))

(defun pprint-clap-asdf (orgsym sym stream)
  (pprint `(defsystem ,sym
             :version "0.0.0"
             :license "New BSD"
             :components
             ((:module ,(format nil "../src/~A"
                                (string-downcase (string orgsym)))
                       :components
                       ((:file ,(string-downcase (string sym)))))))
          stream))

(defun pprint-clap-src (sym stream)
  (pprint `(defpackage ,(intern (string sym) :keyword)
             (:use #:common-lisp)
             (:documentation "fill this documentation"))
          stream))
          
(defun pprint-clap-top-asdf (modules stream)
  (pprint `(defsystem clap
             :version "0.0.0"
             :license "New BSD"
             :depends-on ,modules)
          stream))

(dolist (module *py-modules*)
  (let ((clap-sym (clap-module-sym module)))
    (let ((asdf-file (clap-asdf-file clap-sym))
          (package-file (clap-package-file module clap-sym)))
      (with-open-file (f asdf-file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (pprint-clap-asdf module clap-sym f))
      (ensure-directories-exist
       (format nil "~A/~A/"
               *src-dir* (string-downcase (string module))))
      (with-open-file (f package-file :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (pprint-clap-src clap-sym f)
        ))))

(with-open-file (f (clap-top-asdf-file) :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
  (pprint-clap-top-asdf (mapcar #'clap-module-sym *py-modules*) f))
