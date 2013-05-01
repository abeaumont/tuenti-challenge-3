#!/usr/bin/factor-vm -script
USING: kernel sequences splitting assocs io command-line http.client prettyprint ;
{ "pass[]" "key" } { "" } readln suffix zip "http://pauth.contest.tuenti.net/" http-post nip " " split last print