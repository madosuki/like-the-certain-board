# What's this
This program is like 2ちゃん program.


## Dependent
- roswell
- quicklisp
- caveman2
- flexi-streams
- Ironclad
- [generate-like-certain-board-strings](https://github.com/madosuki/generate-like-certain-board-strings)

## Run
do before first run
```
ros install madosuki/generate-like-certain-board-strings
ros install clack
```

and set path to your .roswell/bin

then
```
./run.sh
```

## settings file path
setting file path is according to BOARD_DB_PATH shell variable.   
DAT file path is according to DAT_DIR_PATH shell variable.   
output HTML from dat when outdate path is according to KAKOLOG_HTML_DIR_PATH shell variable.   
above shell variables is must set.  

