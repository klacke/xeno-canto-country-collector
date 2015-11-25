This is xeno-canto-country-collector

Purpose
-------

To collect seletcted multi-country collections of MP3s from xeno-canto
and provide as one downloadable zip - good for the phone on a trip.



Prerequisites
-------------

This code runs under http://yaws.hyber.org.

Download and and compile mochiweb and ibrowse (from github) and
add something like the following to yaws.conf


    ebin_dir = /home/klacke/git/mochiweb/ebin
    ebin_dir = /home/klacke/git/ibrowse/ebin
    ebin_dir = /home/klacke/git/xeno-canto-country-collector/src

And for the virt server that hosts this, ensure dir_listing
is turned on, e.g

   <server localhost>
                port = 8000
                listen = 0.0.0.0
                docroot = /Users/klacke/yaws/_inst/var/yaws/www
                auth_log = true
                dir_listings = true
   </server>


TODO
----

Add the option of picking 1 song and 1 call instead of
2 of each.

Preview of the contents of the tar file

Add some proper Javascript to create feedback as both the list
is generated as well as the files are downloaded. Hell, maybe add
some CSS too.

Go all in, write an android app that (a) does this and (b) grabs species
data from wikipedia (with pics), creating a localized birding app






