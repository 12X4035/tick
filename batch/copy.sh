
for f in * ;
do
	   	echo ${f%.*}
	for i in *;
		do
			export PGPASSWORD=G7uz3T8p;
			"/Applications/Postgres.app/Contents/Versions/9.6/bin/psql"-p 3432 -U tick_db -h rm-2in2pw90m04p7vwhvgo.pg.rds.aliyuncs.com -d tick -c "CREATE TABLE $i (SELECT * FROM after15 WHERE 1=2);"
		done
done

