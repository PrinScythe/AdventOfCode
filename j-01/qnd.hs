{- Fonctions pour une résolutions générique -}
part1 :: (Num p, Ord a) => [a] -> p
part1 sonarMeasure = case sonarMeasure of
    [] -> 0
    [x] -> 0
    (x:tail) -> inner 0 x tail where
        inner acc prev list = case list of 
            [] -> acc
            (x: tail) -> inner ((if prev < x then 1 else 0) + acc) x tail

part2 :: Num a => [a] -> [a]
part2 sonarMeasure = case sonarMeasure of 
    (x:y:z:t) -> (x+y+z): part2 (y:z:t)
    _ -> []

{- Fonctions pour une résoltion avec le premier DataSet -}
solve1 :: Integer 
solve1 = part1 input1

solve2 :: Integer
solve2 = part1 (part2 input1) 


{- Fonctions pour une résoltion avec le second DataSet -}
solve1' :: Integer 
solve1' = part1 input2

solve2' :: Integer
solve2' = part1 (part2 input2) 

{- DataSet -}
example :: [Integer]
example = [199,200,208,210,200,207,240,269,260,263]

input1 :: [Integer]
input1 = [131,140,136,135,155,175,178,186,187,189,194,195,203,193,178,179,180,188,204,214,215,252,253,261,281,294,293,299,300,301,307,333,324,322,323,335,319,312,313,312,320,323,324,336,341,347,357,358,363,357,334,348,364,365,367,370,369,373,344,328,330,327,339,340,341,335,342,347,349,355,348,359,360,341,342,322,321,324,323,322,324,327,340,341,353,383,387,391,422,424,422,426,429,427,428,445,421,423,429,414,411,407,409,402,422,425,423,427,424,430,428,432,433,434,429,430,406,407,411,420,412,419,422,424,425,426,428,430,432,431,432,433,449,450,453,457,459,462,482,470,471,488,495,496,499,503,516,526,531,532,541,555,554,567,558,557,560,561,566,580,581,582,594,595,598,599,582,589,593,589,592,593,590,606,615,623,627,628,630,636,641,657,658,693,695,698,707,713,714,720,721,723,721,720,707,679,699,676,667,663,664,666,660,663,673,677,662,660,663,683,681,674,689,698,699,704,705,696,697,702,693,694,700,699,681,684,690,669,673,688,691,692,691,688,685,656,638,634,651,665,670,672,685,689,709,711,714,715,735,734,738,737,738,744,765,769,776,809,819,812,814,821,794,793,796,794,815,827,831,834,840,850,846,849,842,847,846,849,850,857,885,887,888,887,883,894,895,913,917,913,915,917,918,920,929,959,961,958,954,956,962,966,959,966,967,969,972,978,993,998,1006,1007,1010,1011,1015,1037,1047,1051,1029,1050,1052,1054,1033,1040,1036,1021,1031,1033,1058,1070,1066,1064,1052,1051,1057,1058,1059,1060,1071,1080,1081,1083,1086,1075,1083,1092,1095,1101,1113,1078,1080,1075,1077,1080,1083,1090,1092,1091,1092,1093,1097,1099,1103,1104,1105,1109,1084,1101,1103,1109,1101,1099,1096,1099,1103,1104,1105,1119,1123,1095,1096,1095,1132,1149,1164,1165,1182,1180,1198,1207,1210,1213,1218,1239,1235,1236,1242,1243,1276,1279,1282,1283,1299,1301,1294,1298,1304,1301,1303,1313,1312,1316,1317,1319,1320,1315,1318,1326,1330,1332,1331,1334,1319,1316,1308,1306,1309,1312,1310,1329,1318,1321,1335,1310,1311,1313,1329,1331,1335,1341,1355,1356,1357,1360,1378,1373,1374,1373,1377,1378,1383,1379,1380,1376,1370,1369,1380,1381,1382,1381,1383,1377,1401,1403,1408,1417,1406,1407,1409,1411,1414,1420,1419,1426,1456,1452,1453,1454,1453,1450,1442,1430,1434,1435,1439,1440,1441,1442,1463,1464,1471,1469,1471,1472,1433,1436,1434,1440,1446,1447,1452,1454,1441,1434,1420,1404,1396,1412,1423,1424,1437,1450,1452,1457,1458,1422,1423,1433,1436,1453,1484,1485,1460,1457,1458,1464,1465,1467,1468,1475,1476,1471,1470,1471,1473,1470,1473,1474,1490,1482,1484,1508,1515,1522,1521,1514,1518,1520,1522,1530,1528,1523,1535,1537,1539,1541,1533,1528,1531,1538,1539,1541,1547,1551,1550,1545,1546,1548,1551,1566,1577,1586,1587,1588,1613,1615,1631,1638,1639,1640,1645,1648,1642,1640,1643,1673,1679,1669,1673,1666,1669,1674,1682,1683,1700,1698,1709,1717,1729,1743,1745,1746,1744,1714,1722,1727,1734,1735,1745,1752,1751,1772,1788,1794,1798,1804,1798,1793,1796,1806,1809,1815,1816,1815,1813,1812,1817,1818,1821,1825,1821,1853,1827,1822,1825,1834,1833,1830,1836,1837,1838,1840,1841,1846,1841,1842,1845,1862,1863,1896,1898,1884,1885,1893,1892,1895,1896,1894,1903,1902,1907,1915,1919,1922,1923,1921,1924,1925,1927,1931,1932,1939,1940,1939,1959,1965,1975,1973,1974,1982,1983,1988,1992,1995,1994,1995,1994,2000,2001,2029,2030,2005,2006,2009,2013,2017,2014,2041,2042,2052,2047,2048,2052,2051,2053,2052,2069,2058,2059,2060,2061,2063,2062,2065,2066,2070,2072,2075,2061,2038,2037,2024,2022,2023,2011,2032,2037,2026,2027,2028,2034,2038,2039,2029,2042,2031,2033,2046,2048,2041,2049,2050,2052,2063,2064,2065,2067,2086,2070,2078,2087,2086,2089,2075,2076,2101,2103,2115,2095,2109,2112,2115,2126,2152,2129,2132,2117,2116,2119,2128,2127,2139,2140,2141,2145,2146,2125,2136,2132,2121,2125,2126,2134,2135,2142,2148,2149,2133,2137,2156,2160,2161,2162,2164,2176,2177,2179,2183,2184,2186,2187,2190,2195,2216,2210,2209,2206,2214,2211,2216,2223,2224,2225,2230,2242,2246,2248,2268,2272,2273,2278,2279,2280,2281,2275,2276,2282,2283,2278,2285,2288,2289,2286,2290,2285,2291,2294,2287,2293,2315,2314,2315,2325,2323,2328,2308,2312,2322,2361,2364,2366,2364,2369,2368,2384,2387,2390,2396,2370,2371,2377,2378,2377,2379,2395,2384,2381,2395,2401,2412,2411,2412,2414,2420,2422,2408,2425,2427,2428,2438,2442,2445,2456,2462,2461,2469,2467,2472,2471,2469,2449,2448,2442,2459,2460,2474,2477,2478,2480,2483,2468,2444,2442,2444,2460,2456,2445,2437,2430,2432,2457,2463,2467,2474,2477,2475,2478,2477,2478,2480,2485,2471,2495,2509,2514,2522,2523,2533,2515,2518,2539,2535,2543,2580,2586,2601,2602,2600,2602,2598,2597,2598,2605,2597,2596,2600,2602,2599,2605,2613,2614,2611,2603,2602,2601,2607,2608,2606,2598,2599,2602,2597,2605,2611,2612,2613,2645,2657,2662,2668,2669,2652,2655,2661,2663,2662,2664,2677,2695,2702,2738,2739,2738,2740,2728,2720,2714,2720,2712,2724,2725,2736,2745,2742,2747,2749,2744,2746,2750,2753,2759,2760,2761,2731,2733,2735,2736,2737,2741,2739,2740,2744,2745,2755,2744,2746,2748,2752,2750,2755,2736,2740,2742,2753,2752,2755,2780,2781,2783,2786,2788,2786,2796,2790,2789,2798,2816,2817,2815,2817,2819,2820,2825,2823,2827,2853,2856,2861,2862,2854,2873,2887,2884,2888,2884,2885,2893,2902,2901,2902,2904,2908,2909,2911,2914,2929,2931,2935,2934,2935,2934,2917,2918,2921,2926,2932,2937,2947,2928,2946,2971,2973,2972,2969,2972,2973,2978,2977,2968,2961,2962,2982,2987,2989,2990,2993,2995,2987,2991,2992,3002,3003,3004,3005,3011,3012,3031,3029,3036,3041,3069,3070,3087,3077,3090,3087,3066,3078,3080,3081,3071,3074,3075,3074,3069,3070,3078,3086,3080,3086,3088,3096,3098,3103,3117,3114,3143,3148,3149,3150,3149,3151,3165,3169,3172,3183,3184,3185,3190,3204,3218,3230,3231,3248,3249,3252,3249,3246,3268,3273,3274,3277,3289,3257,3269,3279,3267,3284,3312,3315,3314,3317,3310,3315,3314,3304,3305,3308,3322,3326,3331,3336,3339,3345,3348,3349,3348,3350,3348,3350,3362,3363,3373,3368,3349,3347,3344,3362,3364,3365,3374,3380,3390,3387,3389,3391,3392,3390,3388,3389,3395,3415,3416,3414,3415,3416,3417,3420,3418,3422,3421,3428,3421,3422,3424,3421,3422,3419,3416,3392,3396,3408,3399,3400,3403,3392,3398,3393,3415,3436,3458,3459,3467,3469,3472,3480,3489,3491,3490,3521,3518,3517,3518,3502,3510,3505,3506,3508,3509,3510,3506,3504,3505,3522,3525,3531,3524,3522,3517,3523,3526,3512,3516,3521,3535,3544,3539,3514,3519,3525,3523,3530,3529,3534,3526,3523,3525,3522,3509,3510,3523,3522,3516,3518,3522,3539,3540,3546,3551,3556,3566,3545,3573,3574,3576,3581,3582,3602,3604,3606,3590,3596,3598,3599,3607,3573,3592,3599,3607,3609,3617,3621,3606,3602,3599,3609,3602,3598,3607,3612,3613,3630,3632,3634,3636,3642,3651,3655,3662,3687,3688,3689,3684,3664,3663,3667,3645,3647,3649,3660,3673,3665,3663,3633,3656,3658,3669,3672,3673,3675,3683,3684,3685,3683,3687,3699,3703,3707,3717,3749,3763,3767,3771,3774,3790,3801,3808,3813,3814,3819,3835,3834,3835,3836,3837,3836,3837,3839,3844,3845,3831,3830,3828,3829,3830,3846,3844,3825,3811,3806,3809,3807,3826,3836,3847,3848,3849,3850,3863,3869,3868,3869,3870,3871,3867,3868,3872,3871,3874,3875,3874,3876,3879,3884,3901,3904,3901,3915,3905,3904,3889,3914,3917,3931,3932,3930,3932,3937,3943,3939,3941,3943,3930,3929,3945,3954,3972,3996,3997,4000,4001,3995,4025,4027,4034,4035,4050,4074,4071,4077,4078,4085,4087,4088,4091,4090,4091,4092,4089,4091,4096,4094,4095,4107,4109,4130,4131,4124,4136,4139,4153,4161,4173,4149,4154,4148,4150,4128,4160,4164,4168,4169,4158,4165,4174,4171,4195,4200,4201,4195,4198,4201,4202,4196,4203,4211,4209,4210,4217,4220,4203,4193,4194,4197,4193,4195,4196,4195,4197,4218,4222,4236,4223,4225,4242,4262,4265,4256,4260,4270,4271,4276,4272,4275,4297,4304,4297,4299,4302,4303,4301,4306,4337,4341,4342,4343,4345,4346,4351,4350,4338,4331,4337,4331,4345,4309,4311,4327,4328,4335,4360,4362,4364,4360,4357,4359,4367,4368,4352,4351,4347,4360,4356,4357,4362,4363,4357,4362,4367,4385,4388,4392,4395,4397,4398,4387,4395,4399,4402,4403,4424,4434,4443,4444,4445,4456,4457,4466,4470,4469,4470,4471,4505,4499,4500,4476,4483,4475,4476,4486,4496,4495,4504,4503,4516,4518,4517,4525,4527,4530,4532,4533,4535,4537,4541,4544,4546,4549,4551,4552,4555,4556,4563,4580,4583,4600,4589,4581,4585,4583,4585,4592,4585,4595,4605,4598,4601,4600,4603,4598,4628,4630,4626,4630,4632,4631,4632,4619,4633,4643,4630,4640,4660,4671,4675,4682,4697,4695,4700,4704,4705,4710,4721,4727,4732,4728,4729,4735,4743,4744,4754,4760,4766,4758,4762,4760,4763,4782,4786,4787,4789,4801,4804,4782,4752,4755,4757,4758,4757,4766,4778,4779,4769,4781,4777,4785,4786,4788,4796,4795,4796,4797,4805,4804,4814,4812,4815,4806,4805,4807,4805,4806,4807,4810,4845,4846,4855,4853,4859,4862,4867,4873,4879,4881,4880,4883,4880,4886,4892,4898,4903,4902,4905,4909,4914,4915,4917,4939,4941,4933,4932,4940,4945,4961,4966,4955,4960,4979,4980,4981,4970,4968,4970,4989,4996,5003,4983,4984,5006,5014,5018,5023,5042,5043,5050,5051,5076,5052,5055,5068,5059,5058,5035,5059,5060,5061,5068,5044,5043,5044,5045,5047,5048,5051,5044,5045,5049,5072,5080,5078,5101,5100,5092,5112,5111,5107,5113,5116,5123,5129,5127,5151,5176,5188,5189,5202,5204,5197,5199,5196,5201,5200,5203,5207,5209,5217,5218,5204,5206,5207,5208,5195,5201,5202,5199,5229,5228,5229,5233,5232,5227,5228,5229,5238,5236,5238,5243,5245,5244,5245,5230,5244,5258,5259,5268,5271,5274,5271,5272,5277,5300,5327,5358,5359,5360,5361,5364,5370,5375,5376,5384,5383,5384,5385,5389,5390,5395,5397,5413,5418,5434,5433,5449,5445,5447,5453,5454,5458,5460,5448,5452,5464,5469,5480,5481,5482,5486,5496,5499,5500,5527,5501,5476,5474,5475,5463,5462,5458,5459,5470,5473,5481,5493,5494,5495,5496,5497,5498,5495,5498,5496,5490,5491,5493,5495,5489,5514,5533,5541,5531,5537,5513,5531,5532,5533,5532,5526,5535,5553,5551,5554,5553,5555,5551,5557,5556,5558,5555,5554,5552,5566,5574,5577,5583,5584,5588,5590,5603,5610,5617,5619,5618,5619,5618,5630,5635,5638,5650,5659,5660,5672,5670,5676,5673,5675,5680,5682,5685,5688,5682,5683,5696,5700,5722,5714,5713,5710,5712,5740,5741,5739,5740,5746,5756,5769,5770,5757,5748,5766,5770]

input2 :: [Integer]
input2 = [123,126,130,137,140,150,155,157,173,186,201,205,234,236,237,252,254,258,265,266,270,278,285,289,310,332,323,349,371,372,373,380,381,382,383,401,406,408,411,412,415,410,413,423,435,440,445,448,453,454,455,470,472,473,476,480,483,485,486,489,491,490,495,497,520,537,547,555,575,577,588,594,595,597,598,599,635,636,637,638,642,649,652,654,655,658,699,700,702,716,718,724,728,729,730,731,734,744,753,754,755,774,775,789,794,795,793,790,809,827,828,831,832,839,840,841,855,856,860,861,867,875,894,901,902,923,931,935,937,944,947,952,961,963,965,985,987,1000,1008,1017,1041,1047,1049,1030,1048,1050,1048,1059,1073,1092,1091,1092,1103,1111,1125,1134,1136,1162,1163,1188,1190,1209,1214,1227,1228,1235,1237,1238,1240,1242,1246,1248,1250,1252,1258,1259,1262,1263,1266,1267,1272,1274,1276,1277,1288,1293,1299,1300,1281,1287,1288,1293,1298,1299,1300,1325,1323,1319,1330,1312,1318,1337,1354,1355,1360,1374,1376,1377,1388,1389,1395,1401,1404,1406,1408,1417,1416,1417,1420,1428,1433,1435,1453,1459,1474,1480,1481,1513,1509,1515,1518,1522,1519,1530,1531,1527,1547,1549,1552,1553,1554,1558,1567,1570,1580,1582,1585,1586,1587,1609,1611,1612,1614,1616,1641,1640,1643,1646,1617,1620,1621,1626,1632,1637,1638,1659,1660,1661,1670,1673,1674,1675,1678,1679,1700,1689,1697,1700,1702,1706,1707,1705,1711,1736,1740,1743,1744,1758,1764,1766,1765,1770,1773,1772,1775,1776,1777,1780,1787,1788,1790,1793,1794,1800,1806,1808,1837,1841,1846,1847,1848,1849,1848,1851,1854,1853,1859,1860,1865,1866,1867,1873,1882,1900,1913,1914,1935,1937,1942,1938,1941,1942,1952,1953,1956,1957,1959,1980,1981,1982,1988,2009,2020,2043,2046,2033,2036,2042,2047,2048,2062,2052,2053,2079,2082,2091,2092,2091,2082,2083,2090,2101,2129,2130,2134,2120,2121,2115,2119,2123,2124,2139,2140,2141,2142,2145,2146,2151,2148,2150,2158,2162,2163,2155,2160,2175,2184,2192,2210,2211,2214,2215,2221,2222,2223,2231,2233,2256,2258,2243,2250,2255,2269,2280,2288,2294,2299,2313,2321,2324,2327,2354,2353,2348,2363,2384,2390,2391,2393,2398,2401,2416,2418,2419,2422,2435,2436,2441,2452,2453,2442,2447,2451,2452,2465,2470,2457,2486,2524,2536,2540,2543,2537,2522,2531,2532,2534,2553,2564,2562,2570,2573,2590,2575,2583,2585,2559,2563,2566,2567,2573,2564,2565,2574,2579,2605,2610,2617,2615,2616,2651,2652,2653,2668,2670,2673,2679,2681,2684,2685,2686,2692,2693,2697,2698,2713,2710,2718,2719,2726,2686,2687,2690,2692,2691,2708,2713,2714,2713,2710,2699,2704,2703,2707,2713,2718,2719,2713,2716,2724,2726,2736,2745,2748,2752,2753,2754,2763,2766,2767,2769,2776,2777,2810,2813,2814,2817,2818,2825,2842,2849,2862,2868,2865,2869,2890,2894,2909,2915,2916,2919,2918,2919,2923,2931,2946,2925,2931,2933,2934,2965,2978,2981,2994,2995,2996,2997,3003,3011,3008,3009,3011,3012,3013,3018,3020,3033,3032,3036,3040,3030,3032,3033,3035,3038,3046,3052,3068,3070,3043,3050,3051,3052,3059,3063,3065,3088,3093,3094,3093,3098,3102,3107,3110,3113,3114,3116,3110,3111,3113,3116,3119,3116,3121,3122,3121,3125,3138,3139,3145,3150,3151,3149,3162,3169,3178,3179,3191,3202,3227,3234,3236,3246,3254,3258,3259,3251,3267,3281,3249,3270,3271,3272,3281,3295,3296,3295,3296,3321,3327,3334,3340,3321,3330,3333,3336,3359,3368,3376,3403,3412,3417,3410,3418,3421,3422,3425,3433,3440,3441,3443,3464,3467,3471,3472,3475,3473,3488,3494,3481,3482,3491,3514,3521,3525,3543,3565,3566,3567,3591,3590,3591,3572,3573,3567,3599,3622,3623,3643,3646,3651,3656,3662,3663,3673,3684,3679,3684,3672,3678,3723,3744,3742,3739,3732,3746,3750,3757,3759,3762,3759,3758,3753,3755,3760,3766,3767,3771,3800,3801,3805,3806,3807,3806,3811,3818,3819,3831,3832,3840,3844,3829,3842,3854,3858,3874,3875,3877,3880,3903,3906,3907,3908,3909,3929,3951,3952,3962,3973,3975,3976,3974,3977,4004,4008,4017,4004,4003,4009,4010,4004,4005,4007,4015,4018,4026,4029,4019,4022,4030,4036,4037,4039,4022,4024,4026,4034,4033,4034,4040,4042,4050,4054,4055,4056,4033,4047,4048,4047,4048,4050,4059,4077,4079,4091,4092,4098,4103,4107,4108,4112,4114,4118,4119,4125,4126,4138,4137,4168,4189,4195,4198,4217,4220,4223,4228,4234,4233,4234,4236,4269,4268,4269,4268,4280,4291,4285,4311,4312,4315,4307,4313,4315,4326,4332,4345,4340,4353,4355,4353,4354,4367,4368,4377,4378,4380,4382,4385,4390,4392,4393,4396,4397,4385,4398,4386,4381,4382,4370,4385,4384,4403,4404,4405,4408,4422,4425,4422,4435,4436,4438,4452,4461,4465,4480,4488,4495,4504,4505,4508,4523,4524,4527,4529,4561,4577,4580,4614,4635,4636,4637,4639,4654,4655,4652,4663,4665,4666,4668,4671,4677,4680,4687,4689,4693,4695,4688,4697,4711,4724,4734,4747,4748,4780,4781,4793,4799,4804,4805,4792,4793,4794,4820,4815,4817,4828,4830,4834,4836,4840,4839,4848,4862,4867,4869,4871,4870,4878,4868,4869,4871,4874,4897,4905,4909,4910,4912,4945,4946,4945,4950,4951,4952,4957,4958,4967,4991,4998,5001,5021,5022,5021,5030,5031,5028,5031,5014,5018,5025,5027,5032,5034,5036,5033,5036,5035,5038,5042,5043,5044,5045,5062,5069,5070,5068,5089,5110,5114,5130,5134,5154,5153,5158,5162,5177,5178,5182,5187,5188,5189,5223,5225,5237,5264,5269,5270,5274,5275,5276,5279,5285,5266,5270,5272,5275,5276,5294,5298,5299,5302,5306,5311,5317,5304,5308,5303,5324,5329,5330,5334,5342,5348,5367,5385,5390,5382,5383,5384,5388,5389,5390,5399,5401,5403,5407,5408,5417,5445,5456,5459,5462,5463,5471,5472,5475,5504,5525,5538,5552,5553,5556,5562,5598,5575,5569,5571,5574,5575,5562,5586,5594,5599,5641,5639,5642,5644,5647,5643,5642,5643,5645,5654,5655,5672,5681,5696,5700,5701,5702,5705,5709,5716,5721,5724,5728,5749,5750,5746,5744,5743,5751,5754,5748,5764,5765,5773,5775,5778,5796,5800,5801,5806,5811,5826,5829,5833,5844,5850,5852,5861,5863,5867,5883,5880,5867,5862,5863,5865,5866,5864,5869,5871,5880,5887,5908,5917,5919,5928,5939,5967,5954,5955,5945,5950,5956,5958,5968,5972,5978,5979,5981,5986,5987,5990,5992,5993,6012,6018,6021,6023,6024,6025,6041,6045,6042,6058,6059,6060,6062,6064,6072,6074,6075,6068,6075,6093,6095,6090,6092,6107,6106,6107,6108,6109,6120,6123,6125,6126,6129,6130,6132,6149,6157,6163,6164,6165,6152,6154,6156,6158,6155,6156,6158,6163,6178,6179,6180,6179,6182,6184,6191,6189,6199,6210,6212,6218,6222,6223,6232,6233,6228,6232,6221,6223,6225,6228,6236,6237,6246,6252,6254,6255,6256,6255,6260,6262,6263,6267,6268,6272,6279,6277,6276,6282,6287,6305,6306,6308,6309,6310,6318,6319,6321,6320,6328,6346,6348,6351,6352,6369,6370,6368,6369,6371,6374,6375,6370,6374,6393,6403,6406,6412,6420,6435,6437,6438,6462,6461,6462,6467,6468,6494,6508,6509,6518,6519,6520,6521,6518,6545,6546,6550,6552,6567,6568,6570,6578,6582,6580,6591,6599,6601,6595,6598,6610,6628,6612,6616,6614,6618,6641,6640,6643,6647,6663,6664,6665,6668,6669,6670,6685,6706,6704,6708,6710,6716,6717,6719,6726,6736,6755,6752,6753,6760,6769,6774,6779,6783,6792,6794,6798,6804,6803,6806,6809,6811,6812,6817,6827,6830,6822,6842,6844,6848,6849,6850,6852,6854,6873,6891,6925,6928,6930,6940,6941,6942,6943,6946,6951,6956,6957,6966,6969,6971,6973,7000,7006,7007,7031,7037,7046,7052,7053,7070,7072,7076,7084,7090,7089,7117,7139,7153,7168,7178,7179,7168,7171,7172,7171,7174,7194,7226,7246,7252,7256,7257,7259,7261,7266,7277,7296,7298,7300,7315,7329,7332,7333,7338,7340,7347,7349,7350,7351,7352,7353,7356,7359,7360,7361,7358,7363,7364,7385,7406,7409,7434,7435,7436,7438,7399,7401,7406,7416,7409,7410,7413,7415,7439,7441,7440,7442,7466,7465,7468,7480,7481,7483,7488,7498,7497,7500,7503,7506,7513,7518,7519,7524,7506,7507,7517,7519,7528,7529,7533,7535,7540,7542,7554,7556,7557,7573,7582,7593,7599,7583,7591,7593,7595,7599,7611,7626,7618,7615,7619,7620,7625,7626,7628,7631,7630,7631,7642,7654,7656,7657,7659,7673,7677,7691,7697,7707,7708,7710,7712,7717,7724,7722,7723,7722,7725,7726,7724,7730,7734,7760,7770,7775,7788,7787,7789,7797,7813,7841,7853,7860,7868,7883,7903,7907,7908,7910,7912,7919,7921,7925,7926,7937,7951,7954,7957,7961,7964,7976,7979,7971,7969,7970,7982,7983,7984,7993,8000,8015,8017,8014,8025,8019,8028,8029,8030,8033,8038,8040,8041,8040,8044,8026,8030,8031,8044,8060,8064,8065,8054,8056,8058,8060,8061,8062,8068,8072,8078,8069,8067,8073,8084,8119,8121,8120,8122,8132,8133,8127,8132,8151,8154,8161,8162,8158,8186,8187,8172,8181,8200,8202,8216,8213,8215,8229,8243,8244,8245,8246,8239,8260,8262,8279,8281,8287,8288,8292,8296,8299,8303,8310,8311,8315,8329,8330,8331,8334,8346,8347,8354,8377,8379,8381,8388,8389,8415,8413,8419,8420,8421,8425,8427,8428,8435,8442,8426,8420,8432,8435,8445,8446,8450,8451,8455,8456,8482,8489,8487,8500,8485,8493,8497,8502,8501,8502,8503,8505,8507,8508,8509,8514,8523,8527,8531,8542,8540,8542,8543,8544,8557,8550,8552,8563,8565,8566,8567,8581,8591,8595,8605,8613,8614,8613,8615,8618,8631,8628,8627,8628,8629,8646,8648,8650,8655,8659,8681,8684,8687,8690,8687,8688,8719,8722,8721,8723,8754,8767,8749,8750,8764,8772,8773,8776,8778,8782,8786,8784,8799,8800,8815,8819,8838,8833,8834,8835,8837,8844,8851,8852,8856,8857,8870,8879,8904,8910,8918,8925,8928,8927,8928,8936,8937,8939,8944,8979,8980,8994,8996,8997,8999,9003,9014,9015,9020,9023,9026,9027,9028,9030,9033,9036,9038,9044,9048,9063,9066,9091,9092,9093,9095,9099,9097,9102,9103,9123,9124,9125,9127,9128,9132,9135,9142,9149,9152,9171,9166,9177,9178,9162,9168,9169,9170,9183,9189,9199,9195,9198,9203,9214,9213,9222,9223,9228,9229,9241,9243,9229,9242,9257,9262,9265,9275,9276,9289,9290,9297,9298,9301,9313,9314,9315,9316,9324,9325,9328,9310,9311,9336,9340,9354,9356,9360,9361,9362,9381,9385,9393,9394,9402,9403,9407,9403,9415,9416,9436,9442,9454,9481,9485,9486,9487,9513,9521,9525,9521,9519,9524,9526,9530,9540,9568,9571,9589,9609,9610,9604,9602,9601,9626,9630,9662,9670,9672,9674,9676,9682,9683,9685,9687,9704,9706,9707,9708,9718,9732,9741,9756,9775,9785,9786,9797,9800,9803,9806,9812,9815,9840,9842,9859,9865,9871,9876,9894,9903,9905,9912,9939,9940,9946,9948,9953,9960,9963,9964,9967,9973,9974,9986,9982,9987,9988,10011,10012,10013,10018,10021,10022,10028,10032,10034,10035,10036,10037,10048,10060,10107,10128,10125,10136,10158,10167,10158,10183,10212,10223,10229,10233,10229,10232,10242,10245,10246,10269,10274,10270,10271,10293,10287,10298,10299,10307,10308,10315,10336,10342,10350,10344,10349,10353,10354,10370,10377,10380,10390,10410,10411]