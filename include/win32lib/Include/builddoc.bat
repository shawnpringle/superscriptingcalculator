@echo off
cd ..\include
eui ..\demo\makedoc.exw win32lib.ew index.htm
move /y *.htm ..\docs

