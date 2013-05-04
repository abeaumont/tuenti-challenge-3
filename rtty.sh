#!/bin/sh

base64 -d - > mp3 && mpg123 -q -w wav mp3 && minimodem --rx -q -f wav rtty && rm mp3 wav
