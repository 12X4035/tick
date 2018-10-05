
for f in * ;
do
	   	echo ${f%.*}
	for i in *;
		do
			export PGPASSWORD=*******;
			"/Applications/Postgres.app/Contents/Versions/9.6/bin/psql"-p 3432 -U tick_db -h *************** -d tick -c "CREATE TABLE $i (SELECT * FROM after15 WHERE 1=2);"
		done
done

