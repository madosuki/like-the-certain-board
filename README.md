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

## setting file path
setting file path is according to BOARD_DB_PATH shell variable. When no set to that variable, to reference to src/db.txt .  
setting file path sample is db-info-sample.txt in this repo root.  

