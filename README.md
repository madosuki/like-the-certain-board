# What's this
This program is like 2ちゃん program.


## Dependents
- roswell
- quicklisp
- caveman2
- flexi-streams
- Ironclad
- cl-markup
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
The setting file path is according to BOARD_SETTING_PATH shell variable.   
Normal DAT file path is according to DAT_DIR_PATH shell variable.   
Kakolog DAT file path is according to KAKOLOG_DAT_DIR_PATH shell variable.  
Output HTML from dat when outdate path is according to KAKOLOG_HTML_DIR_PATH shell variable.   
Above shell variables is rquire.  
Refer to dockerRun.sh  
  
Should check settings_sample.json and SETTING_SAMPLE.txt.  
Rename the former to settings.json then must put that in BOARD_SETTING_PATH/.   
Rename the latter to SETTING.txt then must put that in BOARD_SETTING_PATH/your board name/.   
Above process is need before run app.


## Docker
```
./init_before_do_docker.sh
cp settings_sample.json ../like_the_certain_board_dirs/settings/settings.json
```

must rewrite settings.json to match your environment.

and do
```
./create_board.sh db_user_name db_name
```
Note: that script is example of create board process.  

then
```
docker compose build && docker compose up -d
```
