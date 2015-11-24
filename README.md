This is xeno-canto-country-collector

Purpose
-------

To collect collections of MP3s from xeno-canto.



Prerequisites
-------------

This code runs under http://yaws.hyber.org.

Download and and compile mochiweb and ibrowse (from github) and
add something like the following to yaws.conf

    ebin_dir = /Users/klacke/git/mochiweb/ebin
    ebin_dir = /Users/klacke/git/ibrowse/ebin
    ebin_dir = /Users/klacke/git/xeno/src

And for the virt server that hosts this, ensure dir_listing
is turned on, e.g

   <server localhost>
                port = 8000
                listen = 0.0.0.0
                docroot = /Users/klacke/yaws/_inst/var/yaws/www
                auth_log = true
                dir_listings = true
   </server>



