# How to test API and tree links

## API Server
1. Build API (see README)
2. Run API :
```
/your/home/geneweb/api/gwd_api -hd /your/home/geneweb/distribution/gw -bd  /your/home/geneweb/distribution/bases -max_clients 30 -sig sig_file
```

## Tree-links and API based on samples
1. Build API (see README)
2. Install redis-server (apt-get...)
3. Run redis-server in /your/home/geneweb/test/ in order to user redis.conf and grimaldi.rdb
```
redis-server redis.conf
```
4. Create 2 tests databases (with gwsetup utility): 
    * "grimaldi" with grimladi.ged 
    * "anneburke" with anneburke.ged 
4. Run the API with tree-links options : 
``` 
/your/home/geneweb/api/gwd_api -hd /your/home/geneweb/distribution/gw -bd  /your/home/geneweb/distribution/bases -max_clients 30 -sig sig_file -redis localhost -redis_p 6379 -links_tree_url ^[0-9a-z]:localhost:2322 -p 2322 
```

NB :
* redis must be run locally
* grimaldi.rdb contains samples for connecting Anne BURKE from grimaldi.ged to anneburke.ged

## PHP Web Client
1. Run a simple LAMP server on test/php_cli directory
2. Modify URL_API in index.php if needed
3. Go on your server home url to use a simple form to call API