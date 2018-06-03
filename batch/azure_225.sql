CREATE TABLE table15_after (

	v2 text,

	v5 text,

	v6 int,
	
	v7 text,

	v8 text,

	v10 text,

	v11 int,

	v12 numeric,

	v13 int,

	v14 int,

	v15 int,

	v16 int,

	v17 int,

	v18 numeric,

	v19 int,

	v20 int,

	v21 numeric,

	v22 int,

	v23 int,

  	v28 int,

  	v29 int,

	v30 numeric,

	v31 int,

  	v58 int,

  	v59 int,

	v64 int,

	v65 int,

  	v66 numeric,

  	v67 int,

  	v93 int,

  	v94 int
	
);



SELECT * FROM pg_user;

\copy table15_after FROM '/volumes/HDD2\ 1/data/2015_all_cut.csv' DELIMITER ',' CSV;

CREATE VIEW filter_225 AS (
  SELECT
  *
  FROM
  table16_225
  WHERE
    "v6" IN (1332,1333,1605,1721,1801,1802,1803,1808,1812,1925,1928,1963,2002,2269,2282,2432,2501,2502,2503,2531,2768,2801,2802,2871,2914,3086,3099,3101,3103,3105,3289,3382,3401,3402,3405,3407,3436,3861,3863,3865,4004,4005,4021,4041,4042,4043,4061,4063,4151,4183,4188,4208,4272,4324,4452,4502,4503,4506,4507,4519,4523,4543,4568,4689,4704,4755,4901,4902,4911,5002,5020,5101,5108,5201,5202,5214,5232,5233,5301,5332,5333,5401,5406,5411,5413,5541,5631,5703,5706,5707,5711,5713,5714,5715,5801,5802,5803,5901,6103,6113,6301,6302,6305,6326,6361,6366,6367,6471,6472,6473,6479,6501,6502,6503,6504,6506,6508,6674,6701,6702,6703,6724,6752,6758,6762,6767,6770,6773,6841,6857,6902,6952,6954,6971,6976,6988,7003,7004,7011,7012,7013,7186,7201,7202,7203,7205,7211,7261,7267,7269,7270,7272,7731,7733,7735,7751,7752,7762,7911,7912,7951,8001,8002,8015,8028,8031,8035,8053,8058,8233,8252,8253,8267,8303,8304,8306,8308,8309,8316,8331,8354,8355,8411,8601,8604,8628,8630,8725,8729,8750,8766,8795,8801,8802,8804,8830,9001,9005,9007,9008,9009,9020,9021,9022,9062,9064,9101,9104,9107,9202,9301,9412,9432,9433,9437,9501,9502,9503,9531,9532,9602,9613,9681,9735,9766,9983,9984)
  )



DROP VIEW check_order
CREATE VIEW check_order AS (
    WITH make_date AS (
        SELECT
        to_timestamp("v2"||lpad("v10", 12,'0'), 'YYYYMMDDHH24MISSUS')::timestamp without time zone AS "date",
        *
        FROM
        filter_225
        WHERE
        "v20"<>112 OR "v23"<>112
    )
    SELECT
        *,
        COALESCE("v13", 0) AS "Cv13",
        COALESCE("v28", 0) AS "Cv28",
        COALESCE("v64", 0) AS "Cv64",
        "v18"-lag("v18") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11") AS "Ap",
        "v19"-lag("v19") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11") AS "Av",
        "v21"-lag("v21") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11") AS "Bp",
        "v22"-lag("v22") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11") AS "Bv",
        CASE WHEN "v13"=48 THEN
        lag("v18") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v12"
             WHEN "v13"=16 THEN
        lag("v21") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v12"
               			   ELSE NULL
                           END AS "p_flag",
        CASE WHEN "v13"=48 THEN
        lag("v19") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v15" 
             WHEN "v13"=16 THEN
        lag("v22") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v15"
             			    ELSE NULL
                            END AS "v_flag",
        lag("v18") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v12" AS "Ap_r",
        lag("v19") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v15" AS "Av_r",
        lag("v21") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v12" AS "Bp_r",
        lag("v22") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date","v11")- "v15" AS "Bv_r",
        "v18"-lag("v30") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date", "v11") AS "Ap_1lag",
        "v19"-lag("v31") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date", "v11") AS "Av_1lag",
        "v21"-lag("v66") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date", "v11") AS "Bp_1lag",
        "v22"-lag("v67") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date", "v11") AS "Bv_1lag",
        "date"-lag("date") OVER (partition by  "v2", "v6", "v5"
                                   order by  "v6", "date", "v11") AS "diff_time"
    FROM
        make_date
    ORDER BY
        "v6", "date", "v11"
);


--Cv13=16のとき買い気配に変化
DROP TABLE new_table2016_order;

CREATE VIEW table2016_225_order AS(
    WITH A AS(
        SELECT
        	*,
        ("v22"/10000) AS "bid_vol",
        ("v19"/10000) AS "ask_vol",
        ("v21"*"v22") AS "bid_val",
        ("v18"*"v19") AS "ask_val",
        ("v15"+"Bv") AS "bid_flag",
        ("v15"+"Av") AS "ask_flag",
        (("v18"+"v21")/2) AS "midquote",
        ("v18"-"v21") AS "actual_spread",
        ("v18"-"v21")/(("v18"+"v21")/2) AS "relative_spread"
        FROM
        check_order
    )
    SELECT
    *,
        --約定なし
         (CASE WHEN "p_flag" is null THEN
              (CASE WHEN "Cv64"<>0 THEN 
                   (CASE WHEN "Cv64" < "v19" THEN 2
                         WHEN "Cv64" >= "v19" THEN 1
                         ELSE 999
                   END)
                   ELSE
                   (CASE WHEN "Bp"<0 OR ("Bp"=0 AND "Bv"<0) THEN 5
                         WHEN "Bp"=0 AND "Bv">0 THEN 4
                         WHEN "Bp">0 THEN 3
                         ELSE 998
                         END)
			       END)
			       ELSE
        --更新 （996=約定）価格変化あり
              (CASE WHEN "p_flag" is not null  AND "Cv64"<>0 THEN
                             (CASE WHEN "Cv64" < "v19" THEN 2
                                   WHEN "Cv64" >= "v19" THEN 1
                                   ELSE 997
                                   END)
                    WHEN "Cv13"=16 AND "p_flag" is not null AND "Cv64"=0 THEN
                             (CASE WHEN "Bp_1lag"=0 AND "Bv_1lag"=0 THEN 996
                                   WHEN "Bp_1lag"<0 OR ("Bp_1lag"=0 AND "Bv_1lag"<0) THEN 5
                                   WHEN "Bp_1lag"=0 AND "Bv_1lag">0 THEN 4
                                   WHEN "Bp_1lag">0 THEN 3
                                   ELSE 995
                                   END)
        --更新しない 価格変化なし
					WHEN "p_flag" is null AND "Cv64"<>0 THEN
                            (CASE WHEN "Cv64" < "v19" THEN 2
                                  WHEN "Cv64" >= "v19" THEN 1
                                  ELSE 994
                                  END)
                    WHEN "Cv13"=16 AND "p_flag" is null AND "Cv64"=0 THEN
                             (CASE WHEN "Bp"=0 AND "bid_flag"=0 THEN 993
                                   WHEN ("Bp"=0 AND"bid_flag"<0) OR "Bp"<0 THEN 5
                                   WHEN "Bp"=0 AND "bid_flag">0 THEN 4
                                   WHEN "Bp">0 THEN 3 
                                   ELSE 992
                                   END)
                    ELSE 991 
							END)
                    END) AS "bid_order",
            --Ask_order
             (CASE WHEN "p_flag" is null THEN
                    (CASE WHEN "Cv28"<>0 THEN
                        (CASE WHEN "Cv28" < "v22" THEN 2
                              WHEN "Cv28" >= "v22" THEN 1
                              ELSE 899
                              END)   
                    ELSE
                   (CASE WHEN "Ap">0 OR ("Ap"=0 AND "Av"<0) THEN 5
                            WHEN "Ap"=0 AND "Av">0 THEN 4
                            WHEN "Ap"<0 THEN 3
                            ELSE 898
                            END)    
                    END)
                    ELSE
                     --更新
              (CASE WHEN "p_flag" is not null AND "Cv28"<>0 THEN
                             (CASE WHEN "Cv28" < "v22" THEN 2
                                   WHEN "Cv28" >= "v22" THEN 1                      
                                   ELSE 897
                                   END)
                    WHEN "Cv13"=48 AND "p_flag" is not null AND "Cv28"<>0 THEN
                             (CASE WHEN "Ap_1lag"=0 AND "Av_1lag"=0 THEN 896
                                   WHEN "Ap_1lag">0 OR ("Ap_1lag"=0 AND "Av_1lag"<0) THEN 5
                                   WHEN "Ap_1lag"=0 AND "Av_1lag">0 THEN 4
                                   WHEN "Ap_1lag"<0 THEN 3
                                   ELSE 895
                                   END)
                     --更新しない
                    WHEN "p_flag" is null AND "Cv28"<>0 THEN
                             (CASE WHEN "Cv28" < "v22" THEN 2
                                   WHEN "Cv28" >= "v22" THEN 1 
                                   ELSE 894
                                   END)
                    WHEN "Cv13"=48 AND "p_flag" is null AND "Cv28"<>0 THEN
                             (CASE WHEN "Ap"=0 AND "ask_flag"=0 THEN 893
                                   WHEN ("Ap"=0 AND "ask_flag"<0) OR "Ap">0 THEN 5
                                   WHEN "Ap"=0 AND "ask_flag">0 THEN 4
                                   WHEN "Ap"<0 THEN 3
                                   ELSE 892
                                   END)
                    ELSE 891
                             END)
                    END) 
               AS ask_order
            FROM
            A
            WHERE
                "v20"=128 OR "v20"=131 OR "v23"=128 OR "v23"=131 AND "v13"<>1
            ORDER BY
            "v6", "date", "v11"
            );
SELECT
"v2",
COUNT("v2")
FROM
new_table2016_order_bind_bind
GROUP BY
"v2"

DROP TABLE new_table2016_order


CREATE vIEW table16_test_bind AS(
SELECT
    "date", "v2", "v5", "v6", "v8", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v18", "v19", "v20",
    "v21", "v22", "v23", "v28","v29","v64","v65",
    "diff_time", "bid_vol", "ask_vol","bid_val", "ask_val","midquote", "actual_spread", "relative_spread", "bid_order", "ask_order",
    MAX("v28") OvER (partition by "date", "v6") AS "Mv28",
    MAX("v64") OvER (partition by "date", "v6") AS "Mv64"
FROM
table2016_order_bind
WHERE
"v20"=128 OR "v20"=131 OR "v23"=128 OR "v23"=131 AND "v13"<>1
);



--割合
--998 注文なし
--996 約定あり 更新  注文なし
--993 約定あり 更新なし 注文なし
--
        SELECT
            "bid_order",
            COUNT("bid_order")
        FROM
            table16_test_bind
        GROUP BY
            "bid_order"
        ;

        SELECT
            "ask_order",
            COUNT("ask_order")
        FROM
            table16_test
        GROUP BY
            "ask_order"
        ;

--完成形
CREATE TABLE table16_p_bid AS (
    WITH B AS(
        SELECT
            *,
            CASE WHEN ("v28"is not null) OR ("v64" is not null) THEN
                 (CASE WHEN "v28"="Cv28" OR "v64"="Cv64" THEN 1
                       ELSE 0
                       END)
            ELSE -1
            END AS "flag"
        FROM
            table2016_225_order
    ),
         A AS(
    SELECT
        "date",
        "v2",
        "v6",
        "v5",
        "v8",
        "v10",
        "v11",
        "v13",
        "v14",
        "v15",
        "v16",
        "v18",
        "v19",
        "v28",
        "v29",
        "v21",
        "v22",
        "v64",
        "v65",
        "bid_vol",
        "bid_val",
        "ask_vol",
        "ask_val",
        "midquote",
        "actual_spread",
        "relative_spread",
        "bid_order",
        "flag",
        EXTRACT(second from "date"-lag("date") OvER (partition by  "v2", "v6", "v5"
                                   order by  "date", "v6", "v11")) AS "wait",
		    substring("v8" from 1 for 2) hours,
        substring("v8" from 3 for 2) minutes,
        substring("v8" from 5 for 2) seconds
    FROM
        B
    WHERE
        "bid_order"<10
    ),
    C AS(
    SELECT
        *,
        AvG("wait") OvER (partition by  "v2", "v6", "v5"
                            order by  "date", "v6", "v11"
                        rows between 1 preceding and 1 following) AS "m_wait",
       	"midquote"-lag("midquote") OvER (partition by  "v2", "v6", "v5"
                            		order by  "date", "v6", "v11" )AS "diff_p",
		"midquote"-lag("midquote", 2) OvER (partition by  "v2", "v6", "v5"
                            		order by  "date", "v6", "v11" )AS "diff_p_2",
		"midquote"-lag("midquote", 3) OvER (partition by  "v2", "v6", "v5"
                            		order by  "date", "v6", "v11" )AS "diff_p_3",
		"midquote"-lag("midquote", 4) OvER (partition by  "v2", "v6", "v5"
                            		order by  "date", "v6", "v11" )AS "diff_p_4",
		"midquote"-lag("midquote", 5) OvER (partition by  "v2", "v6", "v5"
                            		order by  "date", "v6", "v11" )AS "diff_p_5",	
    "midquote"-lag("midquote") OvER (partition by  "v2", "v6", "v5", "bid_order"
                            		order by  "date", "v6", "v11" )AS "diff_p_order"		
    FROM
       A
    WHERE
     "flag"<>0
     )
     SELECT
     	ROW_NUMBER() OvER (order by "date") AS row_num,
     	*,
     	AVG("diff_p") OvER (partition by  "v2", "v6", "v5", "hours", "minutes") AS "m_diff_p_1min",
     	AVG("diff_p") OvER (partition by  "v2", "v6", "v5", "hours", "minutes"
                        rows between 1 preceding and 1 following) AS "diff_p_m3",
     	AVG("diff_p") OvER (partition by  "v2", "v6", "v5", "hours", "minutes", "bid_order"
                        rows between 1 preceding and 1 following) AS "diff_p_m3_order"
     FROM
     	C
     ORDER BY
    	"v6", "date", "v11"
);

CREATE vIEW temp AS (
    WITH B AS(
        SELECT
            *,
            CASE WHEN ("v28" is not null) OR ("v64" is not null) THEN
                 (CASE WHEN "v28"="Mv28" OR "v64"="Mv64" THEN 1
                       ELSE 0
                       END)
            ELSE -1
            END AS "flag"
        FROM
            after15_test
        WHERE
        "ask_order"<10
 {   ),
    A AS(
    SELECT
        "date",
        "v2",
        "v6",
        "v5",
        "v11",
        "v18",
        "v19",
        "v28",
        "v29",
        "v21",
        "v22",
        "v64",
        "v65",
        "bid_vol",
        "bid_val",
        "ask_vol",
        "ask_val",
        "midquote",
        "actual_spread",
        "relative_spread",
        "ask_order",
         EXTRACT(second from "date"-lag("date") OvER (partition by  "v2", "v6", "v5"
                                   order by  "date", "v6", "v11")) AS "wait"
    FROM
     B
    LIMIT
    10
    )
    SELECT
        *,
        AvG("wait") OvER (partition by  "v2", "v6", "v5"
                            order by  "date", "v6", "v11"
                        rows between 1 preceding and 1 following) AS "m_wait"
    FROM
       A
    WHERE
    "flag"<>1
    ORDER BY
    "v6", "date", "v11"
)
\copy (
SELECT
"row_num",
"v2",
"v6",
"v5",
"v10",
"v19"::float/10000 AS "ask_vol",
"v22"::float/10000 AS "bid_vol",
"midquote",
"actual_spread",
"relative_spread",
"bid_order",
COALESCE("wait", 0) AS "wait",
COALESCE("m_wait", 0) AS "m_wait",
COALESCE("diff_p", 0) AS "diff_p_1",
COALESCE("diff_p_2", 0) AS "diff_p_2",
COALESCE("diff_p_3", 0) AS "diff_p_3",
COALESCE("diff_p_4", 0) AS "diff_p_4",
COALESCE("diff_p_5", 0) AS "diff_p_5",
COALESCE("diff_p_order", 0) AS "diff_p_order",
"m_diff_p_1min",
"diff_p_m3",
"diff_p_m3_order",
CASE WHEN "v8"::integer >= 90000 AND "v8"::integer < 93000 THEN 1
	 WHEN "v8"::integer >= 93000 AND "v8"::integer < 100000 THEN 2
	 WHEN "v8"::integer >= 100000 AND "v8"::integer < 103000 THEN 3
	 WHEN "v8"::integer >= 103000 AND "v8"::integer < 110000 THEN 4
	 WHEN "v8"::integer >= 110000 AND "v8"::integer <= 113000 THEN 5
	 WHEN "v8"::integer >= 123000 AND "v8"::integer < 130000 THEN 6
	 WHEN "v8"::integer >= 130000 AND "v8"::integer < 133000 THEN 7
	 WHEN "v8"::integer >= 133000 AND "v8"::integer < 140000 THEN 8	 WHEN "v8"::integer >= 140000 AND "v8"::integer < 143000 THEN 9	 WHEN "v8"::integer >= 143000 AND "v8"::integer <= 150000 THEN 10
	 ELSE NULL
	 END AS "time_flag"	 	 	 
FROM
table16_p_bid
)
TO
'/volumes/HDD2 1/data/tb_16_225_res.csv' DELIMITER ',' CSV;

