# Sample Caveman2 on Docker with docker-compose

Read docker-compose.yml and see to Dockerfile in "webapp", "nginx" and "mysql" directories.  


### notice:  

"dockerRun.sh" file path in docker-compose.yml, that's example. if you want to see that, "dockerRun.sh" is where same that docker-compose.yml directory this repository.  
"dockerRun.sh" is very simple shell script file, just start to webapp.  
```
cd /root/.roswell/local-projects/like-the-certain-board/ && APP_ENV=development clackup --server :fcgi --address 0.0.0.0 --port 8888 ./app.lisp
```
