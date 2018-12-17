This is xeno-canto-country-collector

Purpose
-------

To collect selected multi-country collections of MP3s from xeno-canto
and provide as one downloadable zip - good for the phone on a trip.

This code is hosted under http://www.hyber.org/xeno


Prerequisites
-------------

1. I've used iGoTerra to generate the country checklists, these
lists are then parsed by the code in xeno:generate_country_checklists/0

2. Using these checklists, I've then run xeno:dl_xeno/1 to download
all calls according to the rules from xeno-canto

3. Once the calls are in place, and the generated checklists.bin
we can run the whole thing under Yaws.

4. mochiweb and ebrowse (from github) are used to generate the
calls and the checklists.bin files

5. The checklists.bin file, as well as the calls directory
must reside under $DOCROOT/xeno
These files/dirs are separately generated (see 1-4 above)

And for the virt server that hosts this, ensure dir_listing
is turned on, e.g

   <server localhost>
                port = 8000
                listen = 0.0.0.0
                docroot = /Users/klacke/yaws/_inst/var/yaws/www
                auth_log = true
                dir_listings = true
   </server>






