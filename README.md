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
Must setup [roswell](https://github.com/roswell/roswell) and postgresql then  
```bash
ros install madosuki/cl-markup # for from cl-makrup
ros install madosuki/cl-crypt # fork from cl-crypt
ros install madosuki/generate-like-certain-board-strings
ros install madosuki/lack/change_block_app_for_add_control_from_user_agent # forked from lack

ros install clack

```
Then git clone this repo or download zip and extract.  
And do place project dir to ${HOME}/.roswell/local-projects with copy or symbolic.

If maybe failed, refer docker/webapp/Dockerfile .  

And set path to your .roswell/bin .  
then
```bash
./init_before_do_docker.sh # this script is not only for docker.
cp settings_sample.json ../like_the_certain_board_dirs/settings/settings.json
```
```bash
./localRun.sh
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
```bash
./init_before_do_docker.sh
cp settings_sample.json ../like_the_certain_board_dirs/settings/settings.json
```
init_before_do_docker.sh is create dirs above current dir.:
- ../like_the_certain_board_dirs/keys
- ../like_the_certain_board_dirs/nginx_log
- ../like_the_certain_board_dirs/dat
- ../like_the_certain_board_dirs/kakolog/html
- ../like_the_certain_board_dirs/kakolog/dat
- ../like_the_certain_board_dirs/settings

Must rewrite settings.json to match your environment. But if purpose only testing is don't necessary change.  

then
```
docker compose build && docker compose up -d
```

and do that after db started.
```bash
./create_board.sh db_user_name db_name # example: ./create_board.sh user mysite
```
Note: that script is example of create board process.  

then you can accesse localhost:8080.  
