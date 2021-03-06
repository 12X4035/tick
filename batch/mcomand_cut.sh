#!/bin/bash
<<COMMENTOUT
for j in 1332 1333 1605 1721 1801 1802 1803 1808 1812 1925 1928 1963
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/tmp/after_15_bid_1000.csv o=/Volumes/HDD2\ 1/2015_after/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 2002 2269 2282 2432 2501 2502 2503 2531 2768 2801 2802 2871 2914
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/tmp/after_15_bid_2000.csv o=/Volumes/HDD2\ 1/2015_after/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 3086 3099 3101 3103 3105 3289 3382 3401 3402 3405 3407 3436 3861 3863 3865
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/tmp/after_15_bid_3000.csv o=/Volumes/HDD2\ 1/2015_after/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 4004 4005 4021 4041 4042 4043 4061 4063 4151 4183 4188 4208 4272 4324 4452 4502 4503 4506 4507 4519 4523 4543 4568 4689 4704 4755 4901 4902 4911
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/tmp/after_15_bid_4000.csv o=/Volumes/HDD2\ 1/2015_after/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 5002 5020 5101 5108 5201 5202 5214 5232 5233 5301 5332 5333 5401 5406 5411 5413 5541 5631 5703 5706 5707 5711 5713 5714 5715 5801 5802 5803 5901
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/tmp/after_15_bid_5000.csv o=/Volumes/HDD2\ 1/2015_after/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done
COMMENTOUT

for j in 6103 6113 6301 6302 6305 6326 6361 6366 6367 6471 6472 6473 6479 
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_6500.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 6501 6502 6503 6504 6506 6508 6674 6701 6702 6703 6724 6752 6758 6762 6767 6770 6773 6841 6857 6902 6952 6954 6971 6976 6988
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_7000.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 7003 7004 7011 7012 7013 7186 7201 7202 7203 7205 7211 7261 7267 7269 7270 7272
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_7500.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 7731 7733 7735 7751 7752 7762 7911 7912 7951
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_8000.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done


for j in 8001 8002 8015 8028 8031 8035 8053	8058 8233 8252 8253	8267 8303 8304 8306	8308 8309 8316 8331	8354 8355 8411 
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_8500.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 8601 8604 8628 8630 8725 8729 8750 8766 8795 8801 8802 8804 8830 
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_9000.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 9001 9005 9007 9008 9009 9020 9021 9022 9062 9064 9101 9104 9107 9202 9301 9412 9432 9433 9437
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_9500.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done

for j in 9501 9502 9503 9531 9532 9602 9613 9681 9735 9766 9983 9984
do
		mselstr -nfn f=2 v=$j i=/Volumes/HDD2\ 1/2015_after/bid/tmp/after_15_bid_10000.csv o=/Volumes/HDD2\ 1/2015_after/bid/cut_data/after_15_bid_$j.csv;
		echo "$j is done ";
done



