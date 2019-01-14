(def city (make-variable '(ATLANTA BOSTON CHICAGO DALLAS DENVER DETROIT HARTFORD HONOLULU HOUSTON KANSASCITY LOSANGELES NEWORLEANS NEWYORK PORTLAND TUCSON WASHINGTON) :name 'city :role 'L))

(def murder (make-variable '(16.5 4.2 11.6 18.1 6.9 13 2.5 3.6 16.8 10.8 9.7 10.3 9.4 5 5.1 12.5) :name 'murder :role 'Y))

(def rape (make-variable '(24.8 13.3 24.7 34.2 41.5 35.7 8.8 12.7 26.6 43.2 51.8 39.7 19.4 23 22.9 27.6) :name 'rape :role 'Y))

(def robbery (make-variable '(106 122 340 184 173 477 68 42 289 255 286 266 522 157 85 524) :name 'robbery :role 'Y))

(def assault (make-variable '(147 90 242 293 191 220 103 28 186 226 355 283 267 144 148 217) :name 'assault :role 'Y))

(def burglary (make-variable '(1112 982 808 1668 1534 1566 1017 1457 1509 1494 1902 1056 1674 1530 1206 1496) :name 'burglary :role 'Y))

(def larceny (make-variable '(905 669 609 901 1368 1183 724 1102 787 955 1386 1036 1392 1281 756 1003) :name 'larceny :role 'Y))

(def autotheft (make-variable '(494 954 645 602 780 788 468 637 697 765 862 776 848 488 483 739) :name 'autotheft :role 'Y))

(def city-crime (make-dataset (list city murder rape robbery assault burglary
                                    larceny autotheft)
                              :dataset-name 'city-crime))